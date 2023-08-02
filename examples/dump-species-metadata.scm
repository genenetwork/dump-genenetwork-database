#! /usr/bin/env guile
!#

(use-modules (srfi srfi-1)
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



(define (remap-species-identifiers str)
  "This procedure remaps identifiers to standard binominal. Obviously this should
   be sorted by correcting the database!"
  (match str
    ["Fly (Drosophila melanogaster dm6)" "Drosophila melanogaster"]
    ["Oryzias latipes (Japanese medaka)" "Oryzias latipes"]
    ["Macaca mulatta" "Macaca nemestrina"]
    ["Bat (Glossophaga soricina)" "Glossophaga soricina"]
    [str str]))

(define-dump dump-species
  (tables (Species))
  (schema-triples
   (gnc:species rdf:type owl:Class)
   (gnt:name rdfs:domain gnc:species)
   (gnt:binomialName rdfs:domain gnc:species)
   (gnt:family rdfs:domain gnc:species)
   (gnt:organism rdfs:domain gnc:species))
  (triples
      (string->identifier "" (remap-species-identifiers (field Species Fullname))
                          #:separator ""
                          #:proc string-capitalize-first)
    (set rdf:type 'gnc:species)
    (set gnt:name (field Species SpeciesName))
    (set rdfs:label (field Species MenuName))
    (set gnt:binomialName (field Species FullName))
    (set gnt:family (field Species Family))
    (set gnt:organism (ontology 'taxon: (field Species TaxonomyId)))))

(define-dump dump-strain
  (tables (Strain
           (left-join Species "ON Strain.SpeciesId = Species.SpeciesId")))
  (schema-triples
   (gnc:strain rdf:subClassOf gnc:species)
   (gnt:species rdfs:domain gnc:strain)
   (gnt:alias rdfs:domain gnc:strain)
   (gnt:symbol rdfs:domain gnc:strain))
  (triples (string->identifier
            ""
            (regexp-substitute/global
             #f "[^A-Za-z0-9:]"
             (field ("CAST(CONVERT(BINARY CONVERT(Strain.Name USING latin1) USING utf8) AS VARCHAR(15000))" StrainName))
             'pre "_" 'post)
            #:separator ""
            #:proc string-capitalize-first)
    (set rdf:type 'gnc:strain)
    (set gnt:species
         (string->identifier "" (remap-species-identifiers (field Species Fullname))
                             #:separator ""
                             #:proc string-capitalize-first))
    ;; Name, and maybe a second name
    (set rdfs:label (sanitize-rdf-string (field Strain Name)))
    (set rdfs:label (sanitize-rdf-string (field ("IF ((Strain.Name2 != Strain.Name), Strain.Name2, '')" Name2))))
    (set gnt:alias (sanitize-rdf-string (field ("IF ((Strain.Alias != Strain.Name), Strain.Alias, '')" Alias))))
    (set gnt:symbol (field ("IF ((Strain.Symbol != Strain.Name), Strain.Symbol, '')" Symbol)))))

(define-dump dump-mapping-method
  (tables (MappingMethod))
  (schema-triples
   (gnc:mappingMethod rdf:type owl:Class))
  (triples
      (string->identifier "mappingMethod" (field MappingMethod Name))
    (set rdf:type 'gnc:mappingMethod)
    (set rdfs:label (field MappingMethod Name))))

(define-dump dump-inbred-set
  (tables (InbredSet
           (left-join Species "ON InbredSet.SpeciesId=Species.Id")
           (left-join MappingMethod
                       "ON InbredSet.MappingMethodId=MappingMethod.Id")))
  (schema-triples
   (gnc:inbredSet rdf:subClassOf gnc:species)
   (gnt:geneticType rdfs:domain gnc:inbredSet)
   (gnt:code rdfs:domain gnc:inbredSet)
   (gnt:family rdfs:domain gnc:inbredSet)
   (gnt:phenotype rdfs:domain gnc:inbredSet)
   (gnt:genotype rdfs:domain gnt:inbredSet)
   (gnt:mappingMethod rdfs:domain gnc:inbredSet))
  (triples (string->identifier
            "" (field InbredSet Name)
            #:separator ""
            #:proc string-capitalize-first)
    (set rdf:type 'gnc:inbredSet)
    (set rdfs:label (field InbredSet FullName))
    (set gnt:geneticType (field InbredSet GeneticType))
    (set gnt:family (field InbredSet Family))
    (set gnt:mappingMethod (field MappingMethod Name))
    (set gnt:code (field InbredSet InbredSetCode))
    (set gnt:species
         (string->identifier "" (remap-species-identifiers (field Species Fullname))
                          #:separator ""
                          #:proc string-capitalize-first))
    (set gnt:genotype
         (field ("IF ((SELECT PublishFreeze.Name FROM PublishFreeze WHERE PublishFreeze.InbredSetId = InbredSet.Id LIMIT 1) IS NOT NULL, 'Traits and Cofactors', '')" genotypeP)))
    (set gnt:phenotype
         (field ("IF ((SELECT GenoFreeze.Name FROM GenoFreeze WHERE GenoFreeze.InbredSetId = InbredSet.Id LIMIT 1) IS NOT NULL, 'DNA Markers and SNPs', '')" phenotypeP)))))

(define-dump dump-avg-method
  ;; The Name and Normalization fields seem to be the same. Dump only
  ;; the Name field.
  (tables (AvgMethod))
  (schema-triples
   (gnc:avgMethod rdf:type owl:Class))
  (triples (string->identifier "avgmethod" (field AvgMethod Name))
    (set rdf:type 'gnc:avgMethod)
    (set rdfs:label (field AvgMethod Normalization))))



(dump-with-documentation
 (name "Species Metadata")
 (connection %connection-settings)
 (table-metadata? #f)
 (prefixes
  '(("gn:" "<http://genenetwork.org/id/>")
    ("gnc:" "<http://genenetwork.org/category/>")
    ("owl:" "<http://www.w3.org/2002/07/owl#>")
    ("gnt:" "<http://genenetwork.org/term/>")
    ("rdf:" "<http://www.w3.org/1999/02/22-rdf-syntax-ns#>")
    ("rdfs:" "<http://www.w3.org/2000/01/rdf-schema#>")
    ("taxon:" "<http://purl.uniprot.org/taxonomy/>")))
 (inputs
  (list dump-species
        dump-strain
        dump-mapping-method
        dump-avg-method))
 (outputs
  '(#:documentation "./docs/dump-species-metadata.md"
    #:rdf "./verified-data/dump-species-metadata.ttl")))
