"""Complete the UNIZ Trade scenario with the corrected Z and final demand."""

import argparse
import csv
from pathlib import Path
import re
import shutil
import subprocess
import tempfile
from uuid import uuid4
from zipfile import ZipFile
from xml.sax.saxutils import escape

from lxml import etree as ET
import numpy as np


NS = '{http://schemas.openxmlformats.org/spreadsheetml/2006/main}'
NREG, NSEC = 35, 62
N = NREG * NSEC
FINAL = ('HOUSEHOLDS_FINAL_CONSUMPTION_EXPENDITURE',
         'NON-PROFIT_INSTITUTIONS_SERVING_HOUSEHOLDS', 'GENERAL_GOVERNMENT_FINAL_CONSUMPTION',
         'GROSS_FIXED_CAPITAL_FORMATION', 'CHANGE_IN_INVENTORIES_AND_VALUABLES', 'DIRECT_PURCHASES_ABROAD')
ROW = re.compile(rb'<row\b[^>]*\br="([0-9]+)"')
VALUE = re.compile(rb'(<c\b[^>]*\br="([A-Z]+)[0-9]+"[^>]*>)<v>[^<]*</v>(</c>)')


def sheet_path(book, name):
    workbook = ET.fromstring(book.read('xl/workbook.xml'))
    ns = '{http://schemas.openxmlformats.org/officeDocument/2006/relationships}'
    item = next(sheet for sheet in workbook.find(NS+'sheets') if sheet.get('name')==name)
    rels = ET.fromstring(book.read('xl/_rels/workbook.xml.rels'))
    target = next(rel.get('Target') for rel in rels if rel.get('Id')==item.get(ns+'id'))
    return target[1:] if target.startswith('/') else 'xl/'+target


def col_index(name):
    index = 0
    for char in name:
        index = index*26 + char - 64
    return index - 1


def history(path):
    """Read only the historic 210 final-demand columns, preserving signs."""
    result = np.zeros((N, NREG*6))
    with ZipFile(path) as book:
        shared = [''.join(item.itertext()) for item in ET.fromstring(book.read('xl/sharedStrings.xml'))]
        with book.open(sheet_path(book,'MRIO_original')) as stream:
            for _, row in ET.iterparse(stream,events=['end'],tag=NS+'row'):
                index = int(row.get('r'))
                if index>=N+4:
                    break
                if index>=3:
                    for cell in list(row)[-215:]:
                        coord = cell.get('r')
                        j = col_index(coord[:len(coord)-len(str(index))].encode())-(N+2)
                        value = cell.findtext(NS+'v')
                        if not 0<=j<NREG*6 or value is None:
                            continue
                        if index==3:
                            if not shared[int(value)].startswith(FINAL[j%6]):
                                raise ValueError(f'Orden inesperado en la referencia de demanda final, {coord}')
                        else:
                            result[index-4,j]=float(value)
                row.clear()
                while row.getprevious() is not None:
                    del row.getparent()[0]
            if index!=N+4:
                raise ValueError('La MRIO de referencia no contiene las 2170 filas productivas esperadas.')
    return result.reshape(N,NREG,6)


def read_delivery(rscript, path, directory):
    code = ('a <- commandArgs(TRUE); m <- readRDS(a[1]); '
            'stopifnot(identical(dim(m$Z),c(2170L,2170L)),identical(dim(m$Y),c(2170L,105L))); '
            'for (k in c("Z","Y")) writeBin(as.double(t(m[[k]])), file.path(a[2],k),size=8,endian="little")')
    subprocess.run([rscript,'--vanilla','-e',code,str(path),str(directory)],check=True)
    z=np.fromfile(directory/'Z',dtype='<f8').reshape(N,N)
    y=np.fromfile(directory/'Y',dtype='<f8').reshape(N,NREG,3)
    if not np.isfinite(z).all() or not np.isfinite(y).all():
        raise ValueError('La entrega contiene valores no finitos.')
    return z,y


def distribute(y, reference):
    final = np.zeros((N,NREG,6))
    audit = []
    # The three source groups are households, government and capital.
    groups = ((0,1,5),(2,),(3,4))
    for i, columns in enumerate(groups):
        source = y[:,:,i]
        if len(columns)==1:
            final[:,:,columns[0]]=source
            continue
        # Historical components of the same sign, then a documented fallback.
        weights = np.maximum(reference[:,:,columns]*np.sign(source)[...,None],0)
        totals = weights.sum(axis=2)
        fractions = np.divide(weights,totals[...,None],out=np.zeros_like(weights),where=totals[...,None]>0)
        unsupported = (totals==0)&(source!=0)
        fractions[unsupported,0]=1
        if i==2:
            negative = unsupported&(source<0)
            fractions[negative,0]=0
            fractions[negative,1]=1
        final[:,:,columns]=source[...,None]*fractions
        audit.append((f'Grupos sin perfil del mismo signo, uso EMERGING {i+1}',np.count_nonzero(unsupported)))
        audit.append((f'Importe absoluto repartido por defecto, uso EMERGING {i+1}',np.abs(source[unsupported]).sum()))
    error = max(np.max(np.abs(final[:,:,cols].sum(axis=2)-y[:,:,j])) for j,cols in enumerate(groups))
    if error>1e-8:
        raise ValueError('El desglose no conserva los flujos de origen, destino y uso agrupado.')
    return final,audit+[('Residuo máximo al reagrupar los seis usos',error)]


def shares(z, final):
    intermediate=z.reshape(NREG,NSEC,NREG,NSEC)
    final=final.reshape(NREG,NSEC,NREG,6)
    bis=np.zeros((NREG,NSEC,68))
    biso=np.zeros((NREG,NSEC,NREG,68))
    error_bis=error_biso=0
    for destination in range(NREG):
        other=np.arange(NREG)!=destination
        flows=np.concatenate((intermediate[:,:,destination,:],final[:,:,destination,:]),axis=2)
        imported=flows[other].sum(axis=0)
        domestic=flows[destination]
        total=domestic+imported
        active=np.abs(total)>1e-12
        active_import=np.abs(imported)>1e-9
        np.divide(imported,total,out=bis[destination],where=active)
        biso[other,:,destination,:]=np.divide(flows[other],imported,out=np.zeros_like(flows[other]),where=active_import)
        domestic_share=np.divide(domestic,total,out=np.zeros_like(domestic),where=active)
        error_bis=max(error_bis,np.max(np.abs(bis[destination]+domestic_share-active)))
        error_biso=max(error_biso,np.max(np.abs(biso[:,:,destination,:].sum(axis=0)-active_import)))
    if max(error_bis,error_biso)>1e-9:
        raise ValueError('Las participaciones no pasan los controles de suma.')
    audit=[('Error BIS más cuota doméstica',error_bis),('Error suma BISO exterior',error_biso)]
    for name, array in [('BIS',bis),('BISO',biso)]:
        for uses, suffix in [(array[...,:62],'intermedios'),(array[...,62:],'finales')]:
            audit.append((f'{name} {suffix} fuera de [0,1] (tol. 1e-10)',np.count_nonzero((uses< -1e-10)|(uses>1+1e-10))))
    return bis.reshape(N,68),biso.reshape(N*NREG,68),audit


def replace_rows(original, output, table, first, rename):
    """Replace numeric cells only; other worksheet XML is copied verbatim."""
    buffer=b''
    written=0
    while block:=original.read(2**20):
        parts=(buffer+block).split(b'</row>')
        buffer=parts.pop()
        for part in parts:
            part=rename(part)
            match=ROW.search(part)
            if match:
                i=int(match[1])-5
                if 0<=i<len(table):
                    values=table[i]
                    seen=set()

                    def replace(cell):
                        j=col_index(cell[2])-first
                        if not 0<=j<68:
                            return cell[0]
                        seen.add(j)
                        return cell[1]+b'<v>'+format(values[j],'.17g').encode()+b'</v>'+cell[3]

                    part=VALUE.sub(replace,part)
                    if len(seen)!=68:
                        raise ValueError(f'Fila {i+5} de Trade sin sus 68 celdas numéricas.')
                    written+=1
            output.write(part+b'</row>')
    output.write(buffer)
    if written!=len(table):
        raise ValueError(f'Se esperaban {len(table)} filas y se actualizaron {written}.')


def export(template, output, bis, biso):
    with ZipFile(template) as source, ZipFile(output,'x') as target:
        changes={sheet_path(source,'BASE_Import_shares_UNIZ'):(bis,2),
                 sheet_path(source,'EXO_Import_origin_shares_UNIZ'):(biso,3)}
        string_path='xl/sharedStrings.xml'
        string_xml=source.read(string_path)
        labels=[''.join(item.itertext()) for item in ET.fromstring(string_xml)]
        original_label=labels.index('HYDROGEN_PRODUCTION')
        corrected_label='MANUFACTURE_BASIC_METALS'
        if corrected_label in labels:
            corrected_id=labels.index(corrected_label)
        else:
            corrected_id=len(labels)
            string_xml=re.sub(rb'uniqueCount="[0-9]+"',f'uniqueCount="{len(labels)+1}"'.encode(),string_xml,count=1)
            string_xml=string_xml.replace(b'</sst>',b'<si><t>MANUFACTURE_BASIC_METALS</t></si></sst>',1)
        label=re.compile(rb'(<c\b[^>]*\bt="s"[^>]*><v>)'+str(original_label).encode()+rb'(</v></c>)')
        def rename(part):
            return label.sub(lambda match:match[1]+str(corrected_id).encode()+match[2],part)
        for member in source.infolist():
            with source.open(member) as original, target.open(member,'w') as updated:
                if member.filename in changes:
                    replace_rows(original,updated,*changes[member.filename],rename)
                elif member.filename==string_path:
                    updated.write(string_xml)
                else:
                    shutil.copyfileobj(original,updated,2**20)


def column_name(index):
    name=''
    index+=1
    while index:
        index,digit=divmod(index-1,26)
        name=chr(65+digit)+name
    return name


def extend_shares(original, output, values, first):
    columns=[column_name(j) for j in range(first,first+6)]
    added_width=(f'<col min="{first+1}" max="{first+6}" width="30" '
                 'hidden="0" customWidth="1"/>').encode()
    added=0
    buffer=b''
    while block:=original.read(2**20):
        parts=(buffer+block).split(b'</row>')
        buffer=parts.pop()
        for part in parts:
            match=ROW.search(part)
            if match:
                row=int(match[1])
                if row==1:
                    part=re.sub(rb'<dimension ref="[^"]*"\s*/>',
                                f'<dimension ref="A1:{columns[-1]}{len(values)+1}"/>'.encode(),part,count=1)
                    part=part.replace(b'</cols>',added_width+b'</cols>',1)
                    if part.count(b'<c ')!=first:
                        raise ValueError('Las hojas deben contener los 62 usos intermedios de la entrega.')
                    cells=[f'<c r="{col}1" s="1" t="inlineStr"><is><t>{escape(label)}</t></is></c>'
                           for col,label in zip(columns,FINAL)]
                elif 2<=row<=len(values)+1:
                    cells=[f'<c r="{col}{row}" t="n"><v>{value:.17g}</v></c>'
                           for col,value in zip(columns,values[row-2])]
                    added+=1
                else:
                    cells=[]
                part+=''.join(cells).encode()
            output.write(part+b'</row>')
    output.write(buffer)
    if added!=len(values):
        raise ValueError('Faltan filas al insertar la demanda final.')


def workbook_with_trade_ranges(data, trade):
    lookup={'BASE_Import_shares_UNIZ':'BIS_62','EXO_Import_origin_shares_UNIZ':'BISO_62'}
    book=ET.fromstring(data)
    names=book.find(NS+'definedNames')
    if names is None:
        names=ET.Element(NS+'definedNames')
        book.insert(book.index(book.find(NS+'sheets'))+1,names)
    existing={item.get('name') for item in names}
    counts={name:0 for name in lookup.values()}
    source=ET.fromstring(trade.read('xl/workbook.xml'))
    for item in source.iter(NS+'definedName'):
        name=item.get('name','')
        if not name.startswith('BASE_IMPORT_SHARES_') or not name.endswith('_UNIZ'):
            continue
        sheet,_,address=(item.text or '').partition('!')
        if sheet not in lookup:
            continue
        if name in existing or not re.fullmatch(r'\$[A-Z]+\$\d+:\$[A-Z]+\$\d+',address):
            raise ValueError(f'Rango inesperado o ya presente: {name}')
        # Trade starts on row 5 and the coefficient sheets start on row 2.
        adjusted=re.sub(r'\$(\d+)',lambda value:f'${int(value[1])-3}',address)
        ET.SubElement(names,NS+'definedName',name=name).text=f'{lookup[sheet]}!{adjusted}'
        existing.add(name)
        counts[lookup[sheet]]+=1
    if counts!={'BIS_62':70,'BISO_62':4340}:
        raise ValueError(f'Conjunto inesperado de rangos de Trade: {counts}')
    return ET.tostring(book,encoding='UTF-8',xml_declaration=True,standalone=True)


def inline_text(cell, text):
    cell.set('t','inlineStr')
    for child in list(cell):
        cell.remove(child)
    ET.SubElement(ET.SubElement(cell,NS+'is'),NS+'t').text=text


def update_method(data):
    root=ET.fromstring(data)
    rows=root.find(NS+'sheetData')
    inline_text(rows.find(f"{NS}row[@r='6']").find(f"{NS}c[@r='B6']"),
                'La Y de UNIZAR se lee por región en orden hogares, gobierno y formación de capital. El desglose a seis usos se estima con el perfil histórico de WILIAM del mismo signo y conserva los totales originales.')
    inline_text(rows.find(f"{NS}row[@r='7']").find(f"{NS}c[@r='B7']"),
                'TC incluye 62 usos intermedios. BIS y BISO incluyen además seis usos finales a la valoración de la entrega. La referencia a precios de comprador requiere un puente de precios específico. Se conservan los signos.')
    return ET.tostring(root,encoding='UTF-8',xml_declaration=True,standalone=True)


def append_controls(data, audit):
    root=ET.fromstring(data)
    rows=root.find(NS+'sheetData')
    start=max(int(row.get('r')) for row in rows)+1
    for r,(label,value) in enumerate(audit,start):
        row=ET.SubElement(rows,NS+'row',r=str(r),ht='34',customHeight='1')
        inline_text(ET.SubElement(row,NS+'c',r=f'A{r}',s='2'),label)
        cell=ET.SubElement(row,NS+'c',r=f'B{r}',s='2',t='n')
        ET.SubElement(cell,NS+'v').text=format(value,'.17g')
    root.find(NS+'dimension').set('ref',f'A1:B{start+len(audit)-1}')
    return ET.tostring(root,encoding='UTF-8',xml_declaration=True,standalone=True)


def integrate(coefficients, template, output, bis, biso, audit):
    with ZipFile(coefficients) as source, ZipFile(template) as trade, ZipFile(output,'x') as target:
        changes={sheet_path(source,'BIS_62'):(bis[:,62:],64),sheet_path(source,'BISO_62'):(biso[:,62:],65)}
        method=sheet_path(source,'METODO')
        controls=sheet_path(source,'CONTROLES')
        for member in source.infolist():
            with source.open(member) as original, target.open(member,'w') as updated:
                if member.filename in changes:
                    extend_shares(original,updated,*changes[member.filename])
                elif member.filename=='xl/workbook.xml':
                    updated.write(workbook_with_trade_ranges(original.read(),trade))
                elif member.filename==method:
                    updated.write(update_method(original.read()))
                elif member.filename==controls:
                    updated.write(append_controls(original.read(),audit))
                else:
                    shutil.copyfileobj(original,updated,2**20)


def publish_output(source, destination):
    # Create the file beside its destination so it inherits the destination folder's permissions.
    staging = destination.with_name(f'.{destination.name}.{uuid4().hex}.tmp')
    temporary = staging.open('xb')
    try:
        with temporary, source.open('rb') as original:
            shutil.copyfileobj(original, temporary, 2**20)
        staging.replace(destination)
    finally:
        staging.unlink(missing_ok=True)


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--mrio',type=Path,required=True)
    parser.add_argument('--trade',type=Path,default=Path('referencias/Trade.xlsx'))
    parser.add_argument('--referencia-y',type=Path,default=Path('entradas/Calculos limpio MRIO_JN.xlsx'))
    mode=parser.add_mutually_exclusive_group(required=True)
    mode.add_argument('--salida',type=Path,help='Carpeta para generar un Trade independiente')
    mode.add_argument('--coeficientes',type=Path,help='Excel de coeficientes donde incorporar la demanda final')
    parser.add_argument('--rscript',default='Rscript')
    args=parser.parse_args()
    if args.coeficientes:
        with tempfile.TemporaryDirectory(dir=args.coeficientes.parent) as directory:
            scratch=Path(directory)
            z,y=read_delivery(args.rscript,args.mrio,scratch)
            final,allocation=distribute(y,history(args.referencia_y))
            bis,biso,controls=shares(z,final)
            totals=[('Demanda final UNIZAR de tres usos',y.sum()),('Demanda final desglosada en seis usos',final.sum())]
            updated=scratch/args.coeficientes.name
            integrate(args.coeficientes,args.trade,updated,bis,biso,totals+allocation+controls)
            publish_output(updated, args.coeficientes)
        return
    args.salida.mkdir(parents=True,exist_ok=True)
    output=args.salida/'Trade.xlsx'
    audit=args.salida/'Controles_demanda_final.csv'
    if output.exists() or audit.exists():
        raise FileExistsError('Elige una carpeta de salida nueva para Trade.')
    with tempfile.TemporaryDirectory(dir=args.salida) as scratch:
        scratch=Path(scratch)
        z,y=read_delivery(args.rscript,args.mrio,scratch)
        final,allocation=distribute(y,history(args.referencia_y))
        bis,biso,controls=shares(z,final)
        export(args.trade,scratch/'Trade.xlsx',bis,biso)
        with (scratch/audit.name).open('w',encoding='utf-8-sig',newline='') as stream:
            writer=csv.writer(stream,delimiter=';')
            writer.writerow(['Control','Valor'])
            writer.writerows([('Demanda final UNIZAR de tres usos',y.sum()),('Demanda final desglosada en seis usos',final.sum()),*allocation,*controls])
        publish_output(scratch/output.name, output)
        publish_output(scratch/audit.name, audit)
    for row in allocation+controls:
        print(*row)


if __name__=='__main__':
    main()
