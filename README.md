# Semi-automatic Statistical Analysis Platform for Fish Experimental Data

## Data Upload Format Standards

### 1. Quantitative Real-time PCR (qPCR) Data Format

#### Data Structure
**4 columns with fixed column names**:

| Sample_ID | Gene_Name | CT_Value | Group |
|-----------|-----------|----------|-------|
| Sample_01 | GAPDH | 24.56 | Control |
| Sample_02 | GAPDH | 24.89 | Control |
| Sample_03 | IL-6 | 28.34 | Treatment |
| Sample_04 | IL-6 | 27.98 | Treatment |

#### Field Descriptions
- **Sample_ID**: Unique identifier, recommended format: letters + numbers, e.g., `Sample_01`, `Control_02`
- **Gene_Name**: Gene symbol, e.g., `GAPDH`, `β-actin`, `IL-6`, `TNF-α`
- **CT_Value**: Numerical value, with 2 decimal places, typically ranging from 15-35
- **Group**: String type, used to distinguish different treatment groups, e.g., `Control`, `Treatment`, `Stress`, `Normal`

#### Data Requirements
- Must include reference genes (e.g., GAPDH, β-actin)
- At least 3 biological replicates per gene per group
- CT values cannot be NA or negative
- Group names cannot contain special characters

### 2. Regular Experimental Indicators Data Format

#### Data Structure
**3 columns with fixed column names**:

| Sample_ID | Measurement | Group |
|-----------|-------------|-------|
| Fish_01 | 12.56 | Control |
| Fish_02 | 13.12 | Control |
| Fish_03 | 15.89 | Treatment |
| Fish_04 | 14.76 | Treatment |

#### Field Descriptions
- **Sample_ID**: Unique identifier, recommended format: letters + numbers, e.g., `Fish_01`, `GroupA_03`
- **Measurement**: Numerical value, e.g., body weight, body length, enzyme activity, concentration, etc., with 2-3 decimal places
- **Group**: String type, used to distinguish different treatment groups, e.g., `Control`, `Low`, `Medium`, `High`

#### Data Requirements
- At least 3 samples per group
- Measurements cannot be NA or negative
- Group names cannot contain special characters

## Supported File Formats

- Excel files: `.xlsx`, `.xls`
- CSV files: `.csv` (UTF-8 encoding recommended)

## Data Upload Steps

1. Prepare data files according to the above format standards
2. Click on the "Data Upload" tab in the application interface
3. Click the "Select File" button to choose the prepared data file
4. Wait for the data upload to complete and view the data preview
5. Based on the data type, the system will automatically redirect to the corresponding analysis page

## Data Validation

The system will automatically validate uploaded data:
- Check if the number of columns meets requirements (3 or 4 columns)
- Check if data types are correct
- Check for missing values
- Check if groups are reasonable

If data validation fails, the system will display detailed error messages. Please modify the data according to the prompts and re-upload.

## Notes

1. Data file size should not exceed 10MB
2. Avoid using special characters as column names or group names
3. Ensure consistent data format, do not mix different types of data
4. For qPCR data, it is recommended to have at least 3 technical replicates per gene
5. After data upload, the system will automatically save it, and you can view and use it at any time in the analysis page

## Example Data

You can download example data files in the system as templates:
- [qPCR Data Example](example_qpcr_data.xlsx)
- [Regular Data Example](example_regular_data.xlsx)

## Technical Support

If you encounter any problems during data upload or analysis, please contact technical support:

- Email: zhaosicu@163.com
- Phone: 138-XXXX-XXXX

---

**© 2025 Cold Water Fish Health Breeding Team, Sichuan Agricultural University**
**Using AI to build scientific research tools and improve research efficiency**