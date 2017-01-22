all: dokumentacja.pdf

%.pdf: %.md
	pandoc -s $< -o $@

view: dokumentacja.pdf
	zathura $<
