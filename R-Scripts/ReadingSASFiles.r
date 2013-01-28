#To install the package from Github, only needed once
require(devtools)
install_github('sas7bdat', 'biostatmatt')

#Load the package
require(sas7bdat)

str(read.sas7bdat) #Only one function available, takes a file parameter
?read.sas7bdat #Help!

#Read a sas7bdat file
tmp = read.sas7bdat(file.choose())
