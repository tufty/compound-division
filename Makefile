SCHEME = scheme
SCHEME-ARGS = --libdirs /usr/local/lib/chez/lib/
OUTPUT = output

SCHEMEFILES = cincinnati.scm cincinnati-2.scm crouzet.scm emco-browne-sharpe-kertney-trecker.scm hardinge.scm hbm.scm kertney-trecker.scm myford.scm schaublin.scm soba.scm soba-2.scm somua.scm zeatz.scm 

PDFFILES = $(addprefix $(OUTPUT)/, $(notdir $(SCHEMEFILES:.scm=.pdf)))

$(OUTPUT)/%.pdf : %.scm compound-division.scm
	$(SCHEME) $(SCHEME-ARGS) $< compound-division.scm

clean :
	rm -rf *~ $(OUTPUT)/*.log $(OUTPUT)/*.tex $(OUTPUT)/*.aux

all : $(PDFFILES)
