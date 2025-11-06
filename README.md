# Static Gear iVMS and Logbook Cleaner


## Applications
These code files should be used to process iVMS and logbook data from fisheries when the objective is to analyse the spatial distribution and landings data of static gear fishing. The outputs of these code files can also be plugged into the autonomous report generators to get a basic overview of a vessel's activity or the fishery as a whole.
<br />
## Prerequisites
- R 4.2 or higher
<br />


# Using the iVMS and Logbook Cleaner



## Step 1: Logbook Data Structure and Formatting
1. Have a separate logbook file (.csv format) for vessels under 10 m and vessels over 10 m in length. For example:
- *O10_Logbook.csv*
- *U10_Logbook.csv*
2. Ensure the following columns exist and are named accordingly:

<details>
<summary>Under 10 m Logbook</summary>
  
- *Trip Identifier*
- *Vessel Name*
- *Nationality*
- *Fisheries Authority Name*
- *'Administration Port'[Port Code]*
- *'Administration Port'[Port Name]*
- *CFR*
- *RSS No*
- *PLN*
- *Departure Date*
- *'Port of Departure'[Port Name]*
- *'Port of Departure'[Port Code]*
- *Arrival Date*
- *'Port of Arrival'[Port Name]*
- *'Port of Arrival'[Port Code]*
- *FAO Area*
- *ICES*
- *Gear Type*
- *Gear Type Description*
- *Gear Characteristics*
- *FAOSpeciesCode*
- *Species Name*
- *Sum of Weight*
- *SizeDistributionCategory*
- *CatchType*
</details>

<details>
<summary>Over 10 m Logbook</summary>
  
- *Trip.Identifier*
- *Vessel.Name*
- *Nationality*
- *Fisheries.Authority.Name*
- *X.Administration.Port..Port.Code.*
- *X.Administration.Port..Port.Name.*
- *CFR*
- *RSS.No*
- *PLN*
- *Departure.Date*
- *X.Port.of.Departure..Port.Name.*
- *X.Port.of.Departure..Port.Code.*
- *Arrival.Date*
- *X.Port.of.Arrival..Port.Name.*
- *X.Port.of.Arrival..Port.Code.*
- *FAO.Area*
- *ICES*
- *Gear.Type*
- *Gear.Type.Description*
- *SizeDistributionCategory*
- *CatchType*
- *LE_KG_CRE*
- *LE_KG_DGH*
- *LE_KG_LBE* 
- *LE_KG_LOQ*
- *LE_KG_MAC*
- *LE_KG_NEP*
- *LE_KG_POK*
- *LE_KG_POL*
- *LE_KG_RJC*
- *LE_KG_SCR*
- *LE_KG_WHE*
- *LE_KG_HER*
- *Pot_No_CRE*
- *Pot_No_DGH*
- *Pot_No_LBE*
- *Pot_No_LOQ*
- *Pot_No_MAC*
- *Pot_No_NEP*
- *Pot_No_POK*
- *Pot_No_POL*
- *Pot_No_RJC*
- *Pot_No_SCR*
- *Pot_No_WHE*
- *Pot_No_HER*
</details>


3. Assign the directories of the under/over 10 m logbook files to the *under10log_dir* and *over10log_dir* objects in *'iVMS and Logbook Cleaner.R'*. E.g.:
<img width="542" height="39" alt="Screenshot 2025-11-06 at 12 37 14" src="https://github.com/user-attachments/assets/21a76737-66fe-4fb3-8d92-fb84ae4c9261" />
<br />
<br />

## Step 2: iVMS Data Structure and Formatting
1. Make sure all iVMS files are separated by year, in the same folder, and named 'iVMS_{year}.csv'. For example:
<img width="382" height="200" alt="Screenshot 2025-11-06 at 12 19 16" src="https://github.com/user-attachments/assets/07055686-ea45-4a9a-a4a0-1cf90d145676" />
<br />
<br />

2. Ensure that the following columns exist and are named accordingly:

<details>
<summary>iVMS</summary>
  
- *SightingDate*
- *RSSNo*
- *Latitude*
- *Longitude*
- *Course*
- *Speed*
</details>


3. Assign the folder directory of the iVMS files to the *iVMS_dir* object in *'iVMS and Logbook Cleaner.R'*. E.g.:

  <img width="348" height="17" alt="Screenshot 2025-11-06 at 15 06 12" src="https://github.com/user-attachments/assets/db2ff11b-15a7-498d-927a-1602143a9b79" />
<br />
<br />


## Step 3 (optional): Pot Limit Data Structure and Formatting
Pot limit isn't required for the cleaning code to run. However, if desired, it can toggled before code execution to attach respective pot limits to vessel records in the output files. 

1. Set *include_potlimit* to **TRUE** in *'iVMS and Logbook Cleaner.R'*.
2. Ensure that the file has a column named *RSS.No* for merging. Add any additional columns with pot limit info or custom data.  
3. Assign the directory of the pot limit file to the *potlimit_dir* object in *'iVMS and Logbook Cleaner.R'*. E.g.:
<img width="518" height="22" alt="Screenshot 2025-11-06 at 15 31 54" src="https://github.com/user-attachments/assets/798f8dca-224f-409a-bb3c-928a34ee9de9" />
<br />
<br />


## Step 4: Downloading and Adding Shapefiles
Shapefiles are necessary for this code to run, as they are used for clipping iVMS pings that are on land, overlapping mooring sites, or exceeding given distances from shore. As this code was originally used for the Isle of Man fishery, we used 3 nm and 12 nm shapefiles to determine whether or not a vessel was fishing within a either zone. 

<br />
<img width="250" height="250" alt="image" src="https://github.com/user-attachments/assets/86715539-de80-4fa6-b4bc-c5742ca44e68" />
<br />
<br />

If your study site is not the Isle of Man, we suggest reassigning the *nm3_shapefile_dir* (3 nm buffered IoM land shapefile), *nm3_clippedshapefile_dir* (3 nm buffered IoM land shapefile with IoM footprint removed), and *nm12_shapefile_dir* (12 nm buffered IoM land shapefile) with relative shapefiles that reflect your study site.

1. Download the shapefiles from the repository's *Shapefiles* folder.
2. Assign the directories of the _.shp_ files to the respective object names in *'iVMS and Logbook Cleaner.R'*. E.g.:
<img width="704" height="93" alt="Screenshot 2025-11-06 at 15 58 18" src="https://github.com/user-attachments/assets/44ca3be0-6acf-4410-9699-bbe586806b95" />
<br />
<br />

## Step 5: Running the Cleaner
1. In list format, set the desired years for the output files and any vessel codes to be excluded in the code. Save these lists to the objects _years_ and _excluded_vessels_, respectively. E.g.:
<img width="386" height="38" alt="Screenshot 2025-11-06 at 16 26 31" src="https://github.com/user-attachments/assets/ee4f1153-4017-425b-95d6-32f9f7c6adb8" />
<br />
<br />

2. Assign the directory of the _cleaning_script_FM.R_ file to _script_dir_, and the folder directory of the desired output path to _output_dir_.
<img width="395" height="35" alt="Screenshot 2025-11-06 at 16 30 27" src="https://github.com/user-attachments/assets/ed4c7d71-9e22-4d1a-b414-e874d1410996" />
<br />
<br />

3. Run the cleaning script using the code line at the bottom of _iVMS and Logbook Cleaner.R_.

<br />

Once complete, the cleaned and merged datasets should all be saved to your selected output folder directory. Output files will include:
- **u10log_cleaned.csv**; cleaned under 10 m logbook data.
- **O10log_cleaned.csv**; cleaned over 10 m logbook data.
- **combinedlog_O10_U10m.csv**; cleaned and joined logbook data.
- **ivms_cleaned.csv**; cleaned iVMS data.
- **Join5.csv**; joined iVMS and logbook data (all static gear fishing).
- **Join10s_4.csv**; joined iVMS and logbook data (single-species static gear fishing).
- **Join10m_5.csv**; joined iVMS and logbook data (multi-species static gear fishing).

<br />

Please note that this is very early in refinement for studies outside of the Isle of Man static gear fishery.


Feel free to email with any questions!


University email: frm20rbz@bangor.ac.uk

