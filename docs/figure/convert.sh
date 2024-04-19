#!/bin/env bash

#!/bin/bash

# Directorio que contiene las imágenes originales
input_dir="./"

# Directorio de salida para las imágenes convertidas
output_dir="../figure/"

# Tamaño máximo deseado (en píxeles)
max_size="800x800"

# Loop a través de todos los archivos en el directorio de entrada
for input_file in "$input_dir"/*; do
	if [ -f "$input_file" ]; then
		# Obtener el nombre del archivo sin extensión y la extensión
		filename=$(basename "$input_file")
		filename_noext="${filename%.*}"
		file_extension="${filename##*.}"

		# Ruta de salida para la imagen convertida
		output_file="$output_dir/$filename_noext.$file_extension"

		# Convertir y redimensionar la imagen utilizando ImageMagick
		#convert "$input_file" -resize "$max_size" "$output_file"
		convert "$input_file" -quality 80 "$output_file"

		echo "Imagen convertida y redimensionada: $output_file"
	fi
done
