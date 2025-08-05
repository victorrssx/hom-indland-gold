# Homicídios, Terras Indígenas e Reservas de Ouro no Brasil

## Propósito do Estudo

A ideia é investigar a relação causal da eleição de 2018 nas taxas de homicídio dos municípios brasileiros com terras indígenas e reservas auríferas. O objetivo principal é identificar o **efeito causal** da eleição de Bolsonaro sobre a violência através de um exercício de **diferenças em diferenças (DiD)**, utilizando a marcação de terras indígenas e reservas de ouro para delimitar o grupo de tratamento.

## Contexto e Motivação

A literatura sobre conflitos por recursos naturais e direitos territoriais sugere que a sobreposição entre terras indígenas e áreas com potencial mineral pode gerar tensões e aumentar a violência local. Este estudo busca testar empiricamente essa hipótese, controlando por características municipais observáveis e não observáveis.

## Metodologia

- **Período de análise**: 2010-2022
- **Unidade de observação**: Município-ano
- **Método principal**: Diferenças em diferenças (DiD) com efeitos fixos de município e ano
- **Variável dependente**: Taxa de homicídios por 100.000 habitantes
- **Tratamento**: Eleição de Jair Bolsonaro
- **Grupo de Tratamento**: Municíios com presença de terras indígenas e reservas auríferas

## Estrutura dos Dados

### Fontes de Dados

1. **Homicídios**: 
   - DataSUS (Sistema de Informações sobre Mortalidade)
   - Atlas da Violência (IPEA)

2. **Dados Populacionais**: 
   - Instituto Brasileiro de Geografia e Estatística (IBGE)

3. **Terras Indígenas**: 
   - Fundação Nacional dos Povos Indígenas (FUNAI) - Arquivos shapefile com delimitação territorial

4. **Reservas Auríferas**: 
   - Serviço Geológico do Brasil (SGB/CPRM) - Províncias e Distritos Auríferos do Brasil (escala 1:5.000.000)

5. **Dados de Mineração**: 
   - MapBiomas (cobertura e uso da terra)

## Arquivos Técnicos

### Sistema de Coordenadas
- **Projeção**: Sistema de Coordenadas Geográficas
- **Datum**: SIRGAS 2000 (EPSG: 4674)
- **Escala**: Dados auríferos em 1:5.000.000

### Git LFS
Os seguintes tipos de arquivo estão armazenados via Git Large File Storage (LFS) devido ao seu tamanho:

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

## Licença

Este projeto está licenciado sob a **MIT License** - veja o arquivo [LICENSE](LICENSE) para detalhes.

### Licenças dos Dados

- **Dados FUNAI**: Dados públicos disponibilizados pela Fundação Nacional dos Povos Indígenas
- **Dados SGB/CPRM**: Dados públicos do Serviço Geológico do Brasil
- **Dados IBGE**: Dados públicos do Instituto Brasileiro de Geografia e Estatística
- **DataSUS**: Dados públicos do Sistema Único de Saúde
- **Atlas da Violência**: Dados públicos do Instituto de Pesquisa Econômica Aplicada (IPEA)

---

**Autores**: Pedro Hemsley, Victor Líbera  
**Última atualização**: Agosto 2025  
**Licença**: MIT License