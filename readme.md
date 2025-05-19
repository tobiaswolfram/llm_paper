Code for Large Language Models Predict Cognition and Education Close to
or Better than Genomics or Expert Assessment
================

This repository contains the R code to reproduce the analyses presented
in the paper “Large Language Models Predict Cognition and Education
Close to or Better than Genomics or Expert Assessment”. The analysis
workflow is primarily managed using the `targets` package, which
provides a make-like pipeline for R.

## Overview of the Repository

- **`_targets.R`**: Defines the main analysis pipeline using the
  `targets` package. This script outlines all data processing, modeling,
  and evaluation steps as a series of interdependent targets.
- **`R/functions.R`**: Contains all custom R functions used throughout
  the analysis pipeline. Please note that the level of detailed
  documentation within this file may vary.
- **`run.R`**: A convenience script to execute the entire `targets`
  pipeline (e.g., by calling `targets::tar_make()`).
- **`R/get_gpt_embeddings.R`**: Script for generating GPT-3.5 and GPT-4
  based text embeddings from the raw essay data. This process is
  separate from the main `targets` pipeline due to its computational
  intensity or reliance on external APIs.
- **`R/create_data.R`**: This script generates the data and figures
  presented in the manuscript using the results from the completed
  `targets` pipeline.
- **`data/`**: This directory is intended for input data files.
  - `occupation_aspiration_mapping.xlsx`: A manually created mapping for
    classifying occupational aspirations from childhood.
  - `variables.xlsx`: Contains metadata for NCDS variables used in this
    analysis (and potentially others from previous work).
  - `data/camsis/`: Contains mappings of occupations to CAMSIS scores,
    essential for constructing the occupational aspiration variable.

## Data Requirements and Access

To fully reproduce the analyses, several external datasets are required.
Due to data sharing restrictions, not all data can be provided directly
in this repository.

### 1. NCDS Phenotypic and Essay Data

NCDS wave 1-8, as well as the NCDS occupational information cannot be
provided due to data sharing restrictions, but are accessible after
registration at the UK Data Service at
<https://beta.ukdataservice.ac.uk/datacatalogue/series/series?id=2000032>.
Essays of participants, are also provided by the UK Data Service at
<https://beta.ukdataservice.ac.uk/datacatalogue/series/series?id=2000032>.
Put these files into the paths specified in the `_targets.R`-script.

### 2. Essay-Derived Variables (Embeddings, Linguistic Features)

Variables directly derived from the essays (such as text embeddings,
readability scores, spelling metrics, etc.) carry a level of
confidentiality and cannot be shared openly in this repository. Access
for Registered Users: If you have successfully registered with the UK
Data Service and obtained access to the NCDS data, we can share the
derived variables (e.g., the pre-computed text embeddings and linguistic
features) upon request. Please provide evidence of your UKDS NCDS data
access agreement when making such a request. This will save you the
computational step of re-generating these features. Generating them
yourself:\*\* Alternatively, you can generate the GPT-based embeddings
using the provided `get_gpt_embeddings.R` script once you have the raw
essay texts. Other essay-derived features are generated within the main
`targets` pipeline (see `targets_essay` in `_targets.R`) or rely on the
LanguageTool CLI (spelling mistakes) or the tools provided at
<https://www.linguisticanalysistools.org/> (SALAT metrics).

### 3. NCDS Genetic Data

Access to NCDS genetic data is highly restricted and requires a
separate, specific application to the NCDS data access committee and the
UK Data Service, outlining the proposed research. Due to these stringent
access restrictions, we cannot share any raw genetic data or
participant-level polygenic scores. Researchers who obtain the necessary
approvals for NCDS genetic data will need to generate these input files
themselves according to the specifications outlined in our manuscript
and its appendix.

We recognize that many users of this repository may be primarily
interested in the non-genetic aspects of our findings and may not wish
to undertake the complex process of obtaining genetic data. Therefore,
we have substantially restructured the data intake process in this
public codebase. Typically, approved researchers might receive a single
file containing all requested phenotypic variables linked to genetic
identifiers. However, to enhance usability for a broader audience, the
code in this repository has been modified to work directly with the
standard phenotypic data files available from the UK Data Service. This
approach allows users to more easily reproduce the non-genetic analyses
without requiring access to specialized, pre-linked genetic-phenotypic
datasets. It is straightforward to remove or disable the targets related
to genetic data analysis from `_targets.R` if you do not have access to
genetic data or wish to focus solely on the non-genetic models.
