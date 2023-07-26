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



(define-dump dump-genofreeze
  (tables (GenoFreeze
           (left-join InfoFiles "ON InfoFiles.InfoPageName = GenoFreeze.Name")
           (left-join InbredSet "ON GenoFreeze.InbredSetId = InbredSet.InbredSetId"))
          "WHERE GenoFreeze.public > 0 AND GenoFreeze.confidentiality < 1 AND InfoFiles.InfoPageName IS NULL")
  (schema-triples
   (gnt:datasetOfInbredSet rdfs:range gn:inbredSet)
   (gn:genotypeDataset rdfs:subPropertyOf gn:dataset)
   (gnt:shortName rdfs:range rdfs:Literal))
  (triples
      (string->identifier
       ""
       (regexp-substitute/global
        #f "[^A-Za-z0-9:]"
        (regexp-substitute/global
         #f "[^A-Za-z0-9:]"
         (field GenoFreeze Name)
         'pre "_" 'post)
        'pre "_" 'post)
       #:separator ""
       #:proc string-capitalize-first)
    (set rdf:type 'gn:genotypeDataset)
    (set gnt:name (field GenoFreeze Name))
    (set gnt:fullName (field GenoFreeze FullName))
    (set gnt:shortName (field GenoFreeze ShortName))
    (set dct:created (annotate-field
                      (field GenoFreeze CreateTime)
                      '^^xsd:date))
    (set gnt:datasetOfInbredSet
         (string->identifier "" (field InbredSet Name InbredSetName)))))

(define-dump dump-genotypes
  (tables (Geno
           (left-join GenoXRef "ON Geno.Id = GenoXRef.GenoId")
           (left-join GenoFreeze "ON GenoFreeze.Id = GenoXRef.GenoFreezeId")
           (left-join InfoFiles "ON InfoFiles.InfoPageName = GenoFreeze.Name")))
  (schema-triples
   (gn:genotype rdfs:range rdfs:Literal)
   (gnt:genotypeDataset rdfs:subPropertyOf gn:dataset))
  (triples
      (string->identifier
       ""
       (regexp-substitute/global
        #f "[^A-Za-z0-9:]"
        (field ("CONCAT(IF(GenoFreeze.Name IS NULL, '', CONCAT(GenoFreeze.Name, ':')), Geno.Name)" abbrev))
        'pre "_" 'post)
       #:separator ""
       #:proc string-capitalize-first)
    (set rdf:type 'gn:genotype)
    (set gnt:name (sanitize-rdf-string (field Geno Name)))
    (set gnt:markerName (sanitize-rdf-string (field Geno Marker_Name)))
    (set gnt:chr (field Geno Chr))
    (set gnt:mb (annotate-field (field ("IFNULL(Geno.Mb, '')" Mb)) '^^xsd:double))
    (set gnt:sequence (field Geno Sequence))
    (set gnt:source (field Geno Source))
    (set gnt:source2 (field Geno Source2))
    (set gnt:genotypeOfDataset
         (string->identifier
          ""
          (regexp-substitute/global
                     #f "[^A-Za-z0-9:]"
                     (field ("IFNULL(GenoFreeze.Name, '')" DatasetName))
                     'pre "_" 'post)
          #:separator ""
          #:proc string-capitalize-first)
         )
    (set gnt:chrNum
         (annotate-field
          (field ("IFNULL(Geno.chr_num, '')" chr_num))
          '^^xsd:int))
    (set gn:comments (field ("CAST(CONVERT(BINARY CONVERT(Geno.Comments USING latin1) USING utf8) AS VARCHAR(255))" Comments)))
    (set gnt:cM
         (annotate-field
          (field ("IFNULL(GenoXRef.cM, '')" Chr_mm8))
          '^^xsd:int))))



(dump-with-documentation
 (name "Genotype Metadata")
 (connection %connection-settings)
 (table-metadata? #f)
 (prefixes
  '(("dct:" "<http://purl.org/dc/terms/>")
    ("gn:" "<http://genenetwork.org/id/>")
    ("gnt:" "<http://genenetwork.org/term/>")
    ("rdf:" "<http://www.w3.org/1999/02/22-rdf-syntax-ns#>")
    ("rdfs:" "<http://www.w3.org/2000/01/rdf-schema#>")
    ("xsd:" "<http://www.w3.org/2001/XMLSchema#>")))
 (inputs
  (list dump-genofreeze
        dump-genotypes))
 (outputs
  '(#:documentation "./docs/dump-genotype.md"
    #:rdf "./verified-data/dump-genotype.ttl")))
