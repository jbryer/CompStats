require(devtools)
install_github("knitcitations", "cboettig")

require(knitcitations)
ls('package:knitcitations')

#https://github.com/cboettig/knitcitations/blob/master/inst/examples/citations.md

r <- ref(c(RosenbaumRubin1983='10.1093/biomet/70.1.41'))
print(r, style='R')
write.bibtex(r)
citep(r)


