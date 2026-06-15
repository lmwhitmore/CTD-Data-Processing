# CTD-Data-Processing
Processing pipeline and progress tracking for CTD-type data products. 

# If on a windows computer ... 
Open SBEProcessing Tools

# If on a mac

Use the terminal and wine to open SBEProcessing tools. 

brew install --cask wine-stable
xattr -dr com.apple.quarantine /Applications/Wine\ Stable.appDownload SBE Processing Tools from Seabird: link.
 Extract (assuming Downloads folder.
cd ~/Downloads
wine SBEDataProcessing_Win32_V7.26.7-b40.exeThe application is now housed in this directory: ~/.wine/drive_c/Program\ Files\ \(x86\)/Sea-Bird/SBEDataProcessing-Win32

For example, here is how you can run the main startup program for SBE Processing Tools:
wine ~/.wine/drive_c/Program\ Files\ \(x86\)/Sea-Bird/SBEDataProcessing-Win32/SBEDataProc.exeExample of running these from R:
system('wine ~/.wine/drive_c/Program\\ Files\\ \\(x86\\)/Sea-Bird/SBEDataProcessing-Win32/SBEBatch.exe')

system(paste0('wine ~/.wine/drive_c/Program\\ Files\\ \\(x86\\)/Sea-Bird/SBEDataProcessing-Win32/SBEBatch.exe scripts/sbe_v2 ', file.stem))Where file.stem is the file stem name for the cast that you want to process (e.g. cast_001.hex has a stem name of 'cast_001'). The paths themselves are stup in the sbebatch file, here sbe_v2. Note the double \\, one because Mac requires it before special characters like a space. The second because R needs to know that the \ is a real \ and not a special character itself.