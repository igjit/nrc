test:
	docker-compose run --rm r Rscript -e 'devtools::test()'

clean:
	rm -f  *.o tmp*
