Semi-Automatic Statistical Analysis Platform v1.0 for Fish Experimental Data
Data Upload Format Standards
1. Quantitative Real-Time PCR Data Format
Data Structure
Fixed 4-column format with predefined headers:
Sample ID	Gene Name	CT Value	Group
Sample_01	GAPDH	24.56	Control
Sample_02	GAPDH	24.89	Control
Sample_03	IL-6	28.34	Treatment
Sample_04	IL-6	27.98	Treatment
Field Descriptions
Sample ID: Unique identifier, recommended to use alphanumeric format (e.g., Sample_01, Control_02).
Gene Name: Gene symbol (e.g., GAPDH, β-actin, IL-6, TNF-α).
CT Value: Numeric type with 2 decimal places, typically ranging from 15 to 35.
Group: Character type for distinguishing different treatment groups (e.g., Control, Treatment, Stress, Normal).
Data Requirements
Reference genes (e.g., GAPDH, β-actin) must be included.
At least 3 biological replicates are required for each gene in each group.
CT values cannot be NA or negative.
Group names must not contain special characters.
2. General Indicator Analysis Data Format
Data Structure
Fixed 3-column format with predefined headers:
Sample ID	Measurement Value	Group
Fish_01	12.56	Control
Fish_02	13.12	Control
Fish_03	15.89	Treatment
Fish_04	14.76	Treatment
Field Descriptions
Sample ID: Unique identifier, recommended to use alphanumeric format (e.g., Fish_01, GroupA_03).
Measurement Value: Numeric type (e.g., body weight, body length, enzyme activity, concentration) with 2–3 decimal places.
Group: Character type for distinguishing different treatment groups (e.g., Control, Low, Medium, High).
Data Requirements
At least 3 samples are required for each group.
Measurement values cannot be NA or negative.
Group names must not contain special characters.
Supported File Formats
Excel files: .xlsx, .xls
CSV files: .csv (UTF-8 encoding is recommended)
Data Upload Procedures
Prepare a data file that complies with the above format standards.
Navigate to the Data Upload tab on the application interface.
Click the Select File button to choose the prepared data file.
Wait for the upload to complete and check the data preview.
The system will automatically redirect to the corresponding analysis page based on the data type.
Data Validation
The system will automatically validate the uploaded data:
Verify that the number of columns meets the requirements (3 or 4 columns).
Check that the data types are correct.
Detect the presence of missing values.
Validate the rationality of group assignments.
If data validation fails, the system will display detailed error messages. Please modify the data according to the prompts and re-upload.
Notes
The recommended size of the data file should not exceed 10 MB.
Avoid using special characters for column names or group names.
Ensure consistent data formatting; do not mix different data types.
For quantitative real-time PCR data, it is recommended to include at least 3 technical replicates for each gene.
After uploading, the data will be saved automatically, and you can view and use it on the analysis page at any time.
Offline Deployment and Dependency Caching
To ensure that clients can run the software directly in offline environments, all dependent packages need to be pre-installed in the directory BioAnalysis_Plus/installer/library and delivered with the application:
Prepare the R Environment: It is recommended to use the same R version as the client (e.g., R 4.3.x). The dedicated CRAN mirror can be specified by setting the R_CRAN_MIRROR environment variable.
Execute the Caching Script: Run the command Rscript install_missing_packages.R in the Soft1 directory. The script will:
Prioritize the use of built-in offline packages in BioAnalysis_Plus/bin/*.zip.
Automatically install dependencies not provided as zip files via CRAN/Bioconductor (e.g., DESeq2, edgeR, phyloseq, clusterProfiler, DEqMS), and uniformly save them to installer/library.
Use the package list maintained in config.cfg to ensure consistency with the application logic.
Verification and Packaging: Optionally run Rscript check_packages.R to verify the consistency between the list and the cache. After completion, deliver the entire BioAnalysis_Plus directory containing installer/library with the installation package, enabling the client to run the software without an internet connection.
Technical Support
If you encounter any issues during data upload or analysis, please contact technical support:
Email: zhaosicu@163.com
© 2025 Cold-Water Fish Healthy Aquaculture Team, Sichuan Agricultural University. All Rights Reserved.Developing Scientific Research Tools to Improve Research Efficiency
