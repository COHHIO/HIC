# Housing Inventory Chart (HIC)

An open-source HMIS custom reporting script that can use the ServicePoint HUD CSV export to create the HIC files necessary to upload to HDX 1.0.

This repository would be helpful to any HUD-designated Continuum of Care looking for a way to get more out of their HMIS data. This works with the 2019 export and will need to be modified once the 2020 HIC comes around.

### About

This is an open source project released under the GNU AGPLv3 license. See LICENSE for more details or visit the official GNU page at http://www.gnu.org/licenses/agpl-3.0.html.

All the code in this repository is written using R and R Studio. Please consult the book [R for Data Science](https://r4ds.had.co.nz/) for help getting started with R and R Studio.

### Data Sources

This app takes the raw HUD CSV Export files you can get from ServicePoint directly into the project directory. 

You also need the PIT Count for each project, which does not come through in the HUD CSV Export (at least in ServicePoint's.) So in the raw_data folder, there is also a file named PIT2019.csv that just has two columns: ProjectID, and PITCount. Both numeric.

If you clone or fork this, create an "output_data" folder and a "raw_data" folder in your R project. Save your HUD CSV export and PIT Count data files to the raw_data folder, then you can upload the resulting files from the output_data folder to HDX.

### Security

No HMIS data is ever included in this repository. To make this code work, you will need to supply your own HMIS data. You are responsible for securing your HUD CSV export on your computer and ensuring that it is not compromised using whatever security measures you use for that locally.



 