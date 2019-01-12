#! /bin/bash
# from https://github.com/laigor/tabula-java/blob/master/examples/pdftoxls.sh
# 2019-01-11

# changed to 1.0.2 from 
# https://github.com/tabulapdf/tabula-java/releases/tag/v1.0.2
tabula="java -jar /usr/local/bin/tabula-1.0.2-jar-with-dependencies.jar"
pdfpages=$(pdfinfo "${1}" | grep -F 'Pages:' | awk '{print $2}')
pdftable=0
shift=3
fileout="${1%.*}"
for pdfpage in $(seq ${pdfpages}) ; do 
	${tabula} -p ${pdfpage} -g -f JSON "${1}" |
	jq '.[] | .left,.right,.top,.bottom' |
	while read left ; do
		read right
		read top
		read bottom
		echo ${pdfpage} $((${top%.*}-${shift})),$((${left%.*}-${shift})),$((${bottom%.*}+${shift})),$((${right%.*}+${shift}))
	done
done | 
while read pdfpage pdfarea ; do 
		${tabula} -p ${pdfpage} -l -f CSV -a ${pdfarea} "${1}" >> "${fileout}.csv"
done
# don't need to convert to xls
#[ -f "${fileout}.csv" ] && unoconv -i FilterOptions=44,34,UTF-8,1 --format xls "${fileout}.csv" && rm -f "${fileout}.csv"