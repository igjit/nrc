USERID = $(shell id -u)

test:
	docker-compose run --rm r Rscript -e 'devtools::test()'

README.md: README.Rmd
	docker-compose run --rm -u $(USERID) r Rscript -e 'devtools::build_readme()'

clean:
	rm -f  *.o tmp*

.PHONY: test clean
