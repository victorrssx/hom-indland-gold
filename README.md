# Homicides, Indigenous Lands/Territories, and Gold Reserves in Brazil

### Data Sources

1. **Municipalities**:
   - [{geobr} apud IBGE](https://github.com/ipeaGIT/geobr) ✔️

2. **Homicides**: 
   - [DataSUS (Mortality Information System)](http://tabnet.datasus.gov.br/cgi/deftohtm.exe?sim/cnv/obt10br.def) ✔️

3. **Population Data**: 
   - [DataSUS apud IBGE - Population Estimates submitted to TCU for FPM computation](http://tabnet.datasus.gov.br/cgi/tabcgi.exe?ibge/cnv/poptbr.def) ✔️

4. **Indigenous Lands/Territories**: 
   - [National Foundation of Indigenous Peoples (FUNAI) - Geoserver](https://geoserver.funai.gov.br/geoserver/web/wicket/bookmarkable/org.geoserver.web.demo.MapPreviewPage?2&filter=false) ✔️

5. **Gold Reserves**: 
   - Geological Survey of Brazil (SGB/CPRM) - Gold Provinces and Districts of Brazil (scale 1:5,000,000) ✔️

## Data Availability

The table below maps each data source to the corresponding file(s) used in `8. Códigos/Paper.R`.

| Data Source | File Path in Repository |
|---|---|
| Municipal boundaries | Loaded directly via `geobr::read_municipality()` (no local file) |
| Bordering municipalities (IBGE) | `1. Dados Municipais/Municipios Limitrofes - IBGE (2024).xlsx` |
| Predominant biome by municipality (IBGE) | `1. Dados Municipais/Bioma Predominante por Município - IBGE (2024).xlsx` |
| Indigenous lands/territories (FUNAI) | `2. Terras Indígenas - Shapefile/tis_poligonais_portariasPolygon.shp` |
| Gold provinces (SGB/CPRM) | `3. Províncias e Distritos Auríferos do Brasil - Shapefile/Distritos_Provincias/prov_dist_au_provincias_br.shp` |
| Gold districts (SGB/CPRM) | `3. Províncias e Distritos Auríferos do Brasil - Shapefile/Distritos_Provincias/prov_dist_au_distritos_br.shp` |
| Population estimates (DataSUS/IBGE) | `1. Dados Municipais/Estimativa de População IBGE TCU (2010-2022).xlsx` |
| Total homicides by municipality (DataSUS/SIM) | `1. Dados Municipais/Óbitos totais por município - Tabnet DataSUS (2010-2024).xlsx` |

## Technical Files

### Coordinate System
- **Projection**: Geographic Coordinate System
- **Datum**: SIRGAS 2000 (EPSG: 4674)
- **Scale**: Gold data at 1:5,000,000

### Git LFS
The following file types are stored via Git Large File Storage (LFS) due to their size:

```
*.pdf
*.shp
*.cst
*.dbf
*.prj
*.shx
*.cpg
*.sbn
*.sbx
*.shp.xml
*.lyr
*.lyrx
*.qml
*.sld
*.atx
```

## License

This project is licensed under the **MIT License** — see the [LICENSE](LICENSE) file for details.

### Data Licenses

- **FUNAI Data**: Public data made available by the National Foundation of Indigenous Peoples
- **SGB/CPRM Data**: Public data from the Geological Survey of Brazil
- **IBGE Data**: Public data from the Brazilian Institute of Geography and Statistics
- **DataSUS**: Public data from the Brazilian Unified Health System

---

**Authors**: Pedro Hemsley, Romero Rocha, Victor Líbera  
**Last Update**: May 2026  
**License**: MIT License