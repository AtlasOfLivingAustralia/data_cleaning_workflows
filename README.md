# NESP Threatened Species Recovery Hub Project 8.3.1: ‘Fire-affected invertebrates: priority species and management response’.


## Team
Jess Marsh, Payal Bal, Hannah Fraser, Kate Umbers, Aaron Greenville, Libby Rumpff, John Woinarski <br> <br> <br>


## Collaborators
August Hao - data cleaning discussions and scripts <br>
Lee Belbin, Matilda Stevenson – ALA data cleaning discussions and advice on ala4R <br>
Casey Visitin – Collaborator on overlap script using gdal functions <br>
Darren Southwell, Diego Brizuela Torres, David Wilkinson  – SDMs for priority invertebrates <br> <br> <br>


## Contact author
payal.bal@unimelb.edu.au <br> <br> <br>

  
## Workflow in Rscripts
1.	Cleaning AFD checklist: https://github.com/payalbal/nesp_bugs/blob/master/scripts/splist_AFD.R  <br>
  *	Function to remove improper names: https://github.com/payalbal/nesp_bugs/blob/master/scripts/remove_improper_names.R <br>

2.	Download ALA data by phyla: https://github.com/payalbal/nesp_bugs/blob/master/scripts/download_ALA_bytaxon.R <br>
  *	Function to download data by phyla: https://github.com/payalbal/nesp_bugs/blob/master/scripts/get_ala_taxondata.R <br>

  To download ALA data by species: https://github.com/payalbal/nesp_bugs/blob/master/scripts/download_ALA_byspecies.R <br>
  *	Function to download data by species: https://github.com/payalbal/nesp_bugs/blob/master/scripts/get_ala_spdata.R <br>

3.	ALA processing script: https://github.com/payalbal/nesp_bugs/blob/master/scripts/ala_processing1.R <br>
  *	Function to remove improper names: https://github.com/payalbal/nesp_bugs/blob/master/scripts/remove_improper_names.R <br>
  *	Function to find name source and habitat information (author: Matilda Stevenson, ALA): https://github.com/payalbal/nesp_bugs/blob/master/scripts/get_source.R (uses ALA4R function under dev called ala_taxa()) <br>
  *	Function to find AFD synonyms for species names: https://github.com/payalbal/nesp_bugs/blob/master/scripts/get_AFDsynonyms.R <br>

4.	Summary script for cleaned ALA data: https://github.com/payalbal/nesp_bugs/blob/master/scripts/ala_summary.R <br>

5.	Create mask from QGIS output: https://github.com/payalbal/nesp_bugs/blob/master/scripts/ausmask.R <br>

6.	Format, combine and clean individual non-ALA datasheets: https://github.com/payalbal/nesp_bugs/blob/master/scripts/nonala_processing.R <br>

7.	Combine ALA and non-ALA data and format data table: https://github.com/payalbal/nesp_bugs/blob/master/scripts/data_ALAnonALA.R <br>

8.	Correcting species name & updating ALAnonALA data: https://github.com/payalbal/nesp_bugs/blob/master/scripts/data_correction_speciesnames.R <br>

9.	Create polygons using IUCN.eval() function from ConR package for all data: https://github.com/payalbal/nesp_bugs/blob/master/scripts/species_polygons.R <br>
  *	Function based on IUCN.eval() function from ConR package: https://github.com/payalbal/nesp_bugs/blob/master/scripts/conr_iucn_eval.R <br>
  *	To convert alpha hulls in the .rds file into shapefiles: https://github.com/payalbal/nesp_bugs/blob/master/scripts/rds_to_shp_conversion.R <br>

10.	Prepare regional layers: https://github.com/payalbal/nesp_bugs/blob/master/scripts/regions.R  <br>

11.	Region overlaps: https://github.com/payalbal/nesp_bugs/blob/master/scripts/species_regionoverlap.R <br>
  *	Region overlap function: https://github.com/payalbal/nesp_bugs/blob/master/scripts/region_overlap.R <br>

12.	Prepare GEEBAM fire severity layer: https://github.com/payalbal/nesp_bugs/blob/master/scripts/firemap.R <br>

13.	For full extent including offland islands and territories: https://github.com/payalbal/nesp_bugs/blob/master/scripts/firemap_ausextent.R <br>

14.	Fire overlap for all species with point data: https://github.com/payalbal/nesp_bugs/blob/master/scripts/species_points_fireoverlap.R <br>
  *	Points overlap function: https://github.com/payalbal/nesp_bugs/blob/master/scripts/points_overlap.R <br>

15.	Fire overlap for species with polygons: https://github.com/payalbal/nesp_bugs/blob/master/scripts/species_EOO_fireoverlap_paa.R  <br>
  *	Polygon overlap function: https://github.com/payalbal/nesp_bugs/blob/master/scripts/polygon_paa_overlap.R <br>

16.	Combine polygon and point output table: https://github.com/payalbal/nesp_bugs/blob/master/scripts/fireoverlap_table_withPAA.R <br>

17.	Aquatic species overlaps: https://github.com/payalbal/nesp_bugs/blob/master/scripts/aquatic_inverts.R <br>
  *	Slugrisk point overlap function: https://github.com/payalbal/nesp_bugs/blob/master/scripts/points_slugrisk_overlap.R <br>
  *	Slugrisk polygon overlap function: https://github.com/payalbal/nesp_bugs/blob/master/scripts/polygon_slugrisk_overlap.R <br>
 
