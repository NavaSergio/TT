import pandas as pd
import numpy as np
from openpyxl import Workbook

# Cargar archivo Excel
matriz = pd.read_excel("Modif base total secre.xlsx")


# Convertir valores vacíos a 0
matriz.fillna(0, inplace=True)


# Convertir variables categóricas a factores
matriz['Año'] = matriz['Año'].astype('category')
matriz['Sector'] = matriz['Sector'].astype('category')
matriz['No_sector'] = matriz['No_sector'].apply(lambda x: str(x).zfill(2))

# Filtrar datos estatales-sectoriales
matriz_est = matriz[matriz['No_sector'] != "01"].copy()
matriz_total_est = matriz[matriz['No_sector'] == "01"].copy()
matriz_total_sect = matriz[['Año', 'No_sector', 'Sector', 'TOTAL SECTORIAL']].copy()

# Convertir formato largo
matriz_est_long = pd.melt(matriz_est,
                          id_vars=['Año', 'Sector', 'No_sector'],
                          var_name='CVE_ENT',
                          value_name='Pers_ocup')

# Calcular porcentaje de participación estatal por sector
particip_est = matriz_est_long.copy()
#particip_est['Pers_ocup'] = pd.to_numeric(particip_est['Pers_ocup'])

#particip_est['share_est'] = particip_est.groupby(['Año', 'Sector'])['Pers_ocup'].transform(lambda x: round(100 * x / x.sum(), 2))
particip_est['share_est'] = particip_est.groupby(['Año', 'Sector'], observed=False)['Pers_ocup'].transform(
    lambda x: round(100 * x / x.sum(), 2)
)

# Convertir formato ancho
particip_est_wider = particip_est.pivot(index=['Año', 'No_sector', 'Sector'],
                                        columns='CVE_ENT',
                                        values=['Pers_ocup', 'share_est']).reset_index()

# Calcular porcentaje de participación total estatal por sector
total_est_long = pd.melt(matriz_total_est,
                         id_vars=['Año', 'Sector', 'No_sector'],
                         var_name='CVE_ENT',
                         value_name='Pers_ocup')
total_est_long = total_est_long[total_est_long['CVE_ENT'] != 'TOTAL SECTORIAL']

total_est_long['share_est'] = total_est_long.groupby(['Año', 'Sector'], observed=False)['Pers_ocup'].transform(lambda x: round(100 * x / x.sum(), 2))

participtotest_wider = total_est_long.pivot(index=['Año', 'No_sector', 'Sector'],
                                            columns='CVE_ENT',
                                            values=['Pers_ocup', 'share_est']).reset_index()



# Verificación de sumas de participación
verifica = particip_est_wider.filter(like='share_est').sum(axis=1)

# Calcular participación sectorial por estado
particip_sect = matriz_est_long.copy()
particip_sect['share_sect'] = particip_sect.groupby(['Año', 'CVE_ENT'], observed=False)['Pers_ocup'].transform(lambda x: round(100 * x / x.sum(), 2))

# Filtrar matriz de totales sectoriales sin el total global
matriz_totsect_long = matriz_total_sect[matriz_total_sect['No_sector'] != "01"].copy()

# Calcular participación total de cada sector por año
total = matriz_totsect_long.copy()
total['gran_total'] = total.groupby('Año', observed=False)['TOTAL SECTORIAL'].transform('sum')
total['share_sect'] = round(100 * total['TOTAL SECTORIAL'] / total['gran_total'], 2)

# Verificar suma de participaciones
verif_totsect = total.groupby('Año', observed=False)['share_sect'].sum().reset_index()


# Restablecer MultiIndex de las columnas
particip_est_wider.columns = ['_'.join(map(str, col)).strip() if isinstance(col, tuple) else col for col in particip_est_wider.columns]
# Guardar resultados
particip_est_wider.to_excel("./python/Participacion_estatal.xlsx", index=False)
# Restablecer MultiIndex de las columnas
participtotest_wider.columns = ['_'.join(map(str, col)).strip() if isinstance(col, tuple) else col for col in participtotest_wider.columns]
# Guardar resultados
participtotest_wider.to_excel("./python/Participacion_total_estatal.xlsx", index=False)
total.to_excel("./python/Participacion_sectorial.xlsx", index=False)

print("Cálculos completados y resultados exportados a archivos Excel.")
