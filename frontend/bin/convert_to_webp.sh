#!/bin/bash

# Navigate to the images directory
cd ../public/imgs

# Loop through all image files (excluding existing webp files)
for img in *.jpg *.jpeg *.png *.avif *.webp; do
  # Check if the file actually exists (to handle cases where there are no files of a specific extension)
  if [ -f "$img" ]; then
    echo "Converting $img to WebP..."
    
    # Get the filename without extension
    filename="${img%.*}"
    
    # Convert the image to WebP format (temporarily in the same directory)
    ffmpeg -i "$img" -c:v libwebp -quality 85 "temp_${filename}.webp"
    
    # Check if conversion was successful
    if [ -f "temp_${filename}.webp" ]; then
      echo "Successfully converted $img to WebP"
      
      # Move the WebP file to the symptoms directory
      mv "temp_${filename}.webp" "symptoms/${filename}.webp"
      echo "Moved to symptoms/${filename}.webp"
      
      # Delete original file
      echo "Deleting original file $img"
      rm "$img"
    else
      echo "Error: Failed to convert $img to WebP"
    fi
  fi
done

echo "All conversions completed!"
