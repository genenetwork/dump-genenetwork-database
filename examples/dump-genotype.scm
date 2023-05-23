#! /usr/bin/env guile
!#

(use-modules (rnrs programs)
             (rnrs io ports)
             (srfi srfi-1)
             (srfi srfi-26)
             (ice-9 match)
             (ice-9 regex)
             (dump strings)
             (dump sql)
             (dump triples)
             (dump special-forms))



(define %connection-settings
  (call-with-input-file (list-ref (command-line) 1)
    read))

(define %dump-directory
  (list-ref (command-line) 2))



(define-dump dump-genofreeze
  (tables (GenoFreeze
           (left-join InfoFiles "ON InfoFiles.InfoPageName = GenoFreeze.Name")
           (left-join InbredSet "ON GenoFreeze.InbredSetId = InbredSet.InbredSetId"))
          "WHERE GenoFreeze.public > 0 AND InfoFiles.InfoPageName IS NULL")
  (schema-triples
   (gn:datasetOfInbredSet rdfs:range gn:inbredSet)
   (gn:genotypeDataset rdfs:subPropertyOf gn:dataset)
   (gn:shortName rdfs:range rdfs:Literal))
  (triples (ontology
            'dataset:
            (regexp-substitute/global
             #f "[^A-Za-z0-9:]"
             (field GenoFreeze Name)
             'pre "_" 'post))
    (set rdf:type 'gn:genotypeDataset)
    (set gn:name (field GenoFreeze Name))
    (set gn:fullName (field GenoFreeze FullName))
    (set gn:shortName (field GenoFreeze ShortName))
    (set dct:created (annotate-field
                      (field GenoFreeze CreateTime)
                      '^^xsd:date))
    (set gn:datasetOfInbredSet
         (string->identifier "inbredSet" (field InbredSet Name InbredSetName)))))

(define-dump dump-genotypes
  (tables (Geno
           (left-join GenoXRef "ON Geno.Id = GenoXRef.GenoId")
           (left-join GenoFreeze "ON GenoFreeze.Id = GenoXRef.GenoFreezeId")
           (left-join InfoFiles "ON InfoFiles.InfoPageName = GenoFreeze.Name")))
  (schema-triples
   (gn:genotypeDataset rdfs:subPropertyOf gn:dataset))
  (triples
      (ontology
       'genotype:
       (regexp-substitute/global
        #f "[^A-Za-z0-9:]"
        (field ("CONCAT(IF(GenoFreeze.Name IS NULL, '', CONCAT(GenoFreeze.Name, ':')), Geno.Name)" abbrev))
        'pre "_" 'post))
    (set rdf:type 'gn:genotype)
    (set gn:name (sanitize-rdf-string (field Geno Name)))
    (set gn:markerName (sanitize-rdf-string (field Geno Marker_Name)))
    (set gn:chr (field Geno Chr))
    (set gn:mb (annotate-field (field ("IFNULL(Geno.Mb, '')" Mb)) '^^xsd:double))
    (set gn:sequence (annotate-field (field Geno Sequence) '^^xsd:int))
    (set gn:source (field Geno Source))
    (set gn:source2 (field Geno Source2))
    (set gn:genotypeOfDataset
         (ontology 'dataset:
                   (regexp-substitute/global
                    #f "[^A-Za-z0-9:]"
                    (field ("IFNULL(GenoFreeze.Name, '')" DatasetName))
                    'pre "_" 'post)))
    (set gn:chrNum
         (annotate-field
          (field ("IFNULL(Geno.chr_num, '')" chr_num))
          '^^xsd:int))
    (set gn:comments (field ("CAST(CONVERT(BINARY CONVERT(Geno.Comments USING latin1) USING utf8) AS VARCHAR(255))" Comments)))
    (set gn:cM
         (annotate-field
          (field ("IFNULL(GenoXRef.cM, '')" Chr_mm8))
          '^^xsd:int))))



(call-with-target-database
 %connection-settings
 (lambda (db)
   (with-output-to-file (string-append %dump-directory "dump-genotype.ttl")
     (lambda ()
       (prefix "chebi:" "<http://purl.obolibrary.org/obo/CHEBI_>")
       (prefix "dct:" "<http://purl.org/dc/terms/>")
       (prefix "foaf:" "<http://xmlns.com/foaf/0.1/>")
       (prefix "generif:" "<http://www.ncbi.nlm.nih.gov/gene?cmd=Retrieve&dopt=Graphics&list_uids=>")
       (prefix "gn:" "<http://genenetwork.org/>")
       (prefix "hgnc:" "<http://bio2rdf.org/hgnc:>")
       (prefix "homologene:" "<https://bio2rdf.org/homologene:>")
       (prefix "kegg:" "<http://bio2rdf.org/ns/kegg#>")
       (prefix "molecularTrait:" "<http://genenetwork.org/molecular-trait/>")
       (prefix "nuccore:" "<https://www.ncbi.nlm.nih.gov/nuccore/>")
       (prefix "omim:" "<https://www.omim.org/entry/>")
       (prefix "owl:" "<http://www.w3.org/2002/07/owl#>")
       (prefix "phenotype:" "<http://genenetwork.org/phenotype/>")
       (prefix "pubchem:" "<https://pubchem.ncbi.nlm.nih.gov/>")
       (prefix "pubmed:" "<http://rdf.ncbi.nlm.nih.gov/pubmed/>")
       (prefix "rdf:" "<http://www.w3.org/1999/02/22-rdf-syntax-ns#>")
       (prefix "rdfs:" "<http://www.w3.org/2000/01/rdf-schema#>")
       (prefix "taxon:" "<http://purl.uniprot.org/taxonomy/>")
       (prefix "uniprot:" "<http://purl.uniprot.org/uniprot/>")
       (prefix "up:" "<http://purl.uniprot.org/core/>")
       (prefix "xsd:" "<http://www.w3.org/2001/XMLSchema#>")
       (prefix "genotype:" "<http://genenetwork.org/genotype/>")
       (prefix "dataset:" "<http://genenetwork.org/dataset/>")
       (newline)
       (dump-genofreeze db)
       (dump-genotypes db))
     #:encoding "utf8")))
