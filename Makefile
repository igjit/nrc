test:
	Rscript -e 'devtools::test(stop_on_failure = TRUE)'

clean:
	rm -f  *.o tmp*
