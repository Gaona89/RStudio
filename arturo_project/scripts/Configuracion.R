# crear un directorio llamado 'data'
np = "RStudio"

dir.create(np)

#?paste

# crear un subdirectorio llamado 'data'
dir.create(paste(np, '/data', sep = ""))

# Crear subsubdirectorios en el directorio data
dir.create(paste(np, '/data/raw_data', sep = ""))
dir.create(paste(np, '/data/processed_data', sep = ""))
dir.create(paste(np, '/data/metadata', sep = ""))

# crear un subdirectorio llamado 'R_functions'
dir.create(paste(np, '/R_functions', sep = ""))

# crear un subdirectorio llamado 'Rmd'
dir.create(paste(np, '/Rmd', sep = ""))

# crear un subdirectorio llamado 'scripts
dir.create(paste(np, '/scripts', sep = ""))

# crear un subdirectorio llamado 'output'
dir.create(paste(np, '/output', sep = ""))