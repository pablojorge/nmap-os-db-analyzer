# NMap OS DB Analyzer

The original purpose of this project was to play with Haskell and apply it to something (relatively) useful. I came up with the idea of parsing the NMap fingerprints database (I was doing something related to this at work) and then compare and group them by similarity.

This is currently the only analysis performed by this tool.

## Fingerprints Clusters

The program is [clusters.hs](nmap-os-db-analyzer/blob/master/clusters.hs). It expects 3 arguments: input filename, distance threshold, output filename. The input file is the `nmap-os-db` file shipped with [nmap](http://nmap.org/). The threshold specifies how distant can be the fingerprints from each other in order to be grouped together. This means that a higher threshold allows a greater distance, so fewer groups with a higher number of (not so similar) fingerprints will be generated. A smaller threshold will only allow very similar fingerprints to be grouped together.

The program will print how many groups were formed, and dump a graph in graphviz format in the output file given as argument. That graph must be then rendered using the fdp algorithm.

To install graphviz: 

 * Mac OS X (Macports): `sudo port install graphviz`
 * Ubuntu: `sudo apt-get install graphviz`

### Sample

    $ time runhaskell clusters.hs /opt/local/share/nmap/nmap-os-db 17 output.gv
	Clusters count: 156

	real	0m6.743s
	user	0m6.541s
	sys	0m0.218s

    $ time fdp -Tsvgz -o output.svgz output.gv 

	real	0m17.829s
	user	0m17.511s
	sys	0m0.298s

Generated graph:

![Sample graph](nmap-os-db-analyzer/raw/master/sample.svgz)
