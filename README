Pandoc filter for use with Ximera activity service.

To build the filter, make sure you have GHC and Cabal installed and then type:
    cabal install
    cabal build

Example for converting a Pandoc document using the filter from the working directory:
    pandoc --standalone --metadata=repoId:123 --parse-raw -f latex -t html --filter=./ximera-pandoc-filter test.tex

Make sure to pass a repoId in metadata to the filter.

Note that the filter actively writes data to MongoDB for use by the service.  Connection data needs to be stored in `XIMERA_MONGO_URL` and `XIMERA_MONGO_DATABASE`.
