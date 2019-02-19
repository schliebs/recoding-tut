#!/bin/bash
for filename in *.pdf; do
	convert -density 300 "$filename" -quality 90 "${filename%.*}".png
done
