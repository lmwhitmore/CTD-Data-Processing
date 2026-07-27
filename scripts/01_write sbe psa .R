## SBEProcessessing Tools File Set-up
## T.Kelly June 13, 2026

## Use this script to write the paths for SBE Processing tools .psa files and sbe file. 

#### Functions
getpath = function(path) {
  gsub('/', '\\\\', paste0(getwd(), '/', path))
}

generateDataConvertEntry = function(hexPath, psaPath, xmlconPath, outputDir) {
  paste0('datcnv /i', getpath(hexPath), ' /p', getpath(psaPath), ' /c', getpath(xmlconPath), ' /o', getpath(outputDir))
}

generateLoopEditEntry = function(cnvPath, psaPath, outputDir) {
  paste0('loopedit /i', getpath(cnvPath), ' /p', getpath(psaPath), ' /o', getpath(outputDir))
}

generateBinAvgEntry = function(cnvPath, psaPath, outputDir) {
  paste0('binavg /i', getpath(cnvPath), ' /p', getpath(psaPath), ' /o', getpath(outputDir))
}

generateFilterEntry = function(cnvPath, psaPath, outputDir) {
  paste0('filter /i', getpath(cnvPath), ' /p', getpath(psaPath), ' /o', getpath(outputDir))
}

generateCellTMEntry = function(cnvPath, psaPath, outputDir) {
  paste0('celltm /i', getpath(cnvPath), ' /p', getpath(psaPath), ' /o', getpath(outputDir))
}

generateBottleSumEntry = function(rosPath, psaPath, xmlconPath, outputDir) {
  paste0('bottlesum /i', getpath(rosPath), ' /p', getpath(psaPath), ' /c', getpath(xmlconPath), ' /o', getpath(outputDir))
}

runDataConv = function() {
  system('wine ~/.wine/drive_c/Program\\ Files\\ \\(x86\\)/Sea-Bird/SBEDataProcessing-Win32/DatCnvW.exe')
}

runLoopEdit = function() {
  system('wine ~/.wine/drive_c/Program\\ Files\\ \\(x86\\)/Sea-Bird/SBEDataProcessing-Win32/LoopEditW.exe')
}

runBinAvg = function() {
  system('wine ~/.wine/drive_c/Program\\ Files\\ \\(x86\\)/Sea-Bird/SBEDataProcessing-Win32/BinAvgW.exe')
}

runFilter = function() {
  system('wine ~/.wine/drive_c/Program\\ Files\\ \\(x86\\)/Sea-Bird/SBEDataProcessing-Win32/FilterW.exe')
}

runCellTM = function() {
  system('wine ~/.wine/drive_c/Program\\ Files\\ \\(x86\\)/Sea-Bird/SBEDataProcessing-Win32/CellTMW.exe')
}

runBottleSum = function() {
  system('wine ~/.wine/drive_c/Program\\ Files\\ \\(x86\\)/Sea-Bird/SBEDataProcessing-Win32/BottleSumW.exe')
}

runSBE = function() {
  system('wine ~/.wine/drive_c/Program\\ Files\\ \\(x86\\)/Sea-Bird/SBEDataProcessing-Win32/SBEDataProc.exe')
}


## Paths
outputDir = 'proc' # relative to current working directory. It works for absolute
#                     reltive directories, e.g. 'data2', but I don't know if it will 
#                     work for others like '../data' or '/Volumes/User/Data'

## Run everything but bottle summary unless a ros file exists:
tmpFilePath = gsub(' ', 'T', gsub(':', '_', Sys.time()))

hexFiles = list.files('raw', pattern = '.hex')

for (i in 1:length(hexFiles)) { 
  
  hex = hexFiles[i]
  name = strsplit(hexFiles[i], '.hex')[[1]][1]
  
  lines = c(
    generateDataConvertEntry(
      hexPath = paste0('raw/',hex), #raw/NABOS_01_01.hex'
      psaPath = 'scripts/DatCnv.psa',
      xmlconPath = paste0('raw/',name,'.XMLCON'), #'raw/NABOS_01_01.XMLCON',
      outputDir = outputDir),
    generateFilterEntry(
      cnvPath = paste0(outputDir, '/', name, '.cnv'), # paste0(outputDir, '/NABOS_01_01.cnv'),
      psaPath = 'scripts/Filter.psa',
      outputDir = outputDir),
    generateCellTMEntry(
      cnvPath = paste0(outputDir, '/', name, '.cnv'), # paste0(outputDir, '/NABOS_01_01.cnv'),
      psaPath = 'scripts/CellTM.psa',
      outputDir = outputDir),
    generateLoopEditEntry(
      cnvPath = paste0(outputDir, '/', name, '.cnv'), # paste0(outputDir, '/NABOS_01_01.cnv')
      psaPath = 'scripts/LoopEdit.psa',
      outputDir = outputDir),
    generateBinAvgEntry(
      cnvPath = paste0(outputDir, '/', name, '.cnv'), # paste0(outputDir, '/NABOS_01_01.cnv'),
      psaPath = 'scripts/BinAvg.psa',
      outputDir = outputDir)
  )
  
  writeLines(
    con = tmpFilePath,
    lines,
    sep = '\n'
  )
  
system(paste0('wine ~/.wine/drive_c/Program\\ Files\\ \\(x86\\)/Sea-Bird/SBEDataProcessing-Win32/SBEBatch.exe ', tmpFilePath))
  
  ## delete tmpFile? 
  
  ## if ros file exists, then run bottle summary.
  # I don't have data to actually test this with, so this is the most likely part to break.
  if (file.exists(paste0(outputDir,'/',name,'.ros'))) { 
    lines = generateBottleSumEntry(
      rosPath = paste0(outputDir, '/',name,'.ros'), #/NABOS_01_01.ros'),
      psaPath = 'scripts/BottleSum.psa',
      xmlconPath = paste0('raw/',name,'.XMLCON'),
      outputDir = outputDir)
    
    writeLines(
      con = tmpFilePath,
      lines,
      sep = '\n'
    )
    
    system(paste0('wine ~/.wine/drive_c/Program\\ Files\\ \\(x86\\)/Sea-Bird/SBEDataProcessing-Win32/SBEBatch.exe ', tmpFilePath))
  }
  #delete tmpfile
}
  