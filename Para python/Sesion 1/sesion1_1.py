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
matriz_est1 = matriz_est.loc[:,matriz_est.columns != "TOTAL SECTORIAL"].copy()

matriz_total_est = matriz[matriz['No_sector'] == "01"].copy()
matriz_total_sect = matriz[['Año', 'No_sector', 'Sector', 'TOTAL SECTORIAL']].copy()

# Convertir formato largo
matriz_est_long = pd.melt(matriz_est1,
                          id_vars=['Año', 'Sector', 'No_sector'],
                          var_name='CVE_ENT',
                          value_name='Pers_ocup')

# Ordenar por 'Año', 'No_sector' y 'CVE_ENT'
matriz_est_long = matriz_est_long.sort_values(by=['Año', 'No_sector', 'CVE_ENT']).reset_index(drop=True)


# Calcular porcentaje de participación estatal por sector
particip_est = matriz_est_long.copy()



particip_est['share_est'] = particip_est.groupby(['Año', 'Sector'], observed=False)['Pers_ocup'].transform(
    lambda x: round(100 * x / x.sum(), 2)
)


matriz_totest_long = (
    matriz_total_est
    .melt(id_vars=["Año", "No_sector", "Sector"], 
          var_name="CVE_ENT", 
          value_name="Pers_ocup")
    .query('CVE_ENT != "TOTAL SECTORIAL"')
)




particip_totest = (
    matriz_totest_long
    .groupby(["Año", "Sector"], observed=False)
    .apply(lambda group: group.assign(
        share_est=round(100 * group["Pers_ocup"] / group["Pers_ocup"].sum(), 2)
    ))
    .reset_index(drop=True)  # Restablece el índice
)



# Convertir formato ancho
participest_wider = particip_est.pivot(index=['Año', 'No_sector', 'Sector'],
                                        columns='CVE_ENT',
                                        values=['Pers_ocup', 'share_est']).reset_index()

participtotest_wider = particip_totest.pivot(
    index=['Año', 'Sector', 'No_sector'],  # Columnas que permanecen como índice
    columns='CVE_ENT',                     # Columnas a expandir
    values=['Pers_ocup', 'share_est']      # Columnas cuyos valores se usan
).reset_index()


# Calcular porcentaje de participación total estatal por sector
#total_est_long = pd.melt(matriz_total_est,
#                         id_vars=['Año', 'Sector', 'No_sector'],
#                         var_name='CVE_ENT',
#                         value_name='Pers_ocup')
#total_est_long = total_est_long[total_est_long['CVE_ENT'] != 'TOTAL SECTORIAL']

#total_est_long['share_est'] = total_est_long.groupby(['Año', 'Sector'], observed=False)['Pers_ocup'].transform(lambda x: round(100 * x / x.sum(), 2))

#participtotest_wider = total_est_long.pivot(index=['Año', 'No_sector', 'Sector'],
#                                            columns='CVE_ENT',
#                                            values=['Pers_ocup', 'share_est']).reset_index()



# Verificación de sumas de participación
verifica = participest_wider.filter(like='share_est').sum(axis=1)
print(verifica)

# Calcular participación sectorial por estado
particip_sect = matriz_est_long.copy()
particip_sect['share_sect'] = particip_sect.groupby(['Año', 'CVE_ENT'], observed=False)['Pers_ocup'].transform(lambda x: round(100 * x / x.sum(), 2))

# Ordenar el archivo de participación sectorial
#particip_sect = particip_sect.sort_values(by=['CVE_ENT', 'Año', 'No_sector']).reset_index(drop=True)

# Verificar la contribución sectorial
verif_part_sect = (
    particip_sect
    .groupby(['Año', 'CVE_ENT'], as_index=False, observed=False)
    .agg(verifica=('share_sect', 'sum'))
)

# Ordenar el DataFrame de verificación
verif_part_sect = verif_part_sect.sort_values(by=['Año', 'CVE_ENT']).reset_index(drop=True)

# Filtrar matriz de totales sectoriales sin el total global
matriz_totsect_long = matriz_total_sect[matriz_total_sect['No_sector'] != "01"].copy()

# Calcular participación total de cada sector por año
total = matriz_totsect_long.copy()
total['gran_total'] = total.groupby('Año', observed=False)['TOTAL SECTORIAL'].transform('sum')
# total['share_sect'] = round(100 * total['TOTAL SECTORIAL'] / total['gran_total'], 2)

# Verificar suma de participaciones
#verif_totsect = total.groupby('Año', observed=False)['share_sect'].sum().reset_index()


 # calcula la participacion total de cada sector por año
particip_totsect = (
    total
    .assign(share_sect=lambda df: round(100 * df["TOTAL SECTORIAL"] / df["gran_total"], 2))
)
# verifica que la suma de participaciones sea 100. La variación es por redondeos.
verif_totsect = (
    particip_totsect
    .groupby("Año", as_index=False, observed=False)
    .agg(verifica=("share_sect", "sum"))
)

participsect_wider = particip_sect.pivot(
    index=["Año", "Sector", "No_sector"],  # Columnas que permanecen como índice
    columns="CVE_ENT",                     # Nombres de las nuevas columnas
    values=["Pers_ocup", "share_sect"]     # Valores que se distribuyen en las nuevas columnas
).reset_index()

# Selecciona las columnas correspondientes de cada DataFrame
participest_subset = participest_wider.iloc[:, [0, 1, 2] + list(range(35, 67))]
participtotest_subset = participtotest_wider.iloc[:, [0, 1, 2] + list(range(35, 67))]

# Une por renglones (concatena los DataFrames)
tabla_share_est = pd.concat([participest_subset, participtotest_subset], ignore_index=True)

#cambia nombres de columnas por nombres de los estados 
tabla_share_est.columns = [
    "Año", "No_sector", "Sector", "Aguascalientes", "Baja California",
    "Baja California Sur", "Campeche", "Coahuila", "Colima", "Chiapas",
    "Chihuahua", "CDMX", "Durango", "Guanajuato", "Guerrero", "Hidalgo",
    "Jalisco", "Mexico", "Michoacan", "Morelos", "Nayarit", "Nuevo Leon",
    "Oaxaca", "Puebla", "Queretaro", "Quintana Roo", "San Luis Potosi",
    "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz",
    "Yucatan", "Zacatecas"
]
  ## Guardar en archivo excel 
 # deben tener instalado Java , pero no estoy segura si solo funciona en Mac 
 # tambien pueden intentar instalar la  librería openxlsx
tabla_share_est.to_excel("./python/Participacion estatal.xlsx", index=False)



# Paso 1: Regresa la base a formato ancho
participsect_wider = particip_sect.pivot(
    index=["Año", "Sector", "No_sector"],
    columns="CVE_ENT",
    values=["Pers_ocup", "share_sect"]
).reset_index()

# Paso 2: Aplanar las columnas
participsect_wider.columns = [
    f"{col[0]}_{col[1]}" if col[1] else col[0] for col in participsect_wider.columns
]

# Paso 3: Selecciona las columnas deseadas
# Selecciona las columnas necesarias de participsect_wider y particip_totsect
participsect_subset = participsect_wider.iloc[:, [0, 1, 2] + list(range(35, 67))]
participtotsect_subset = particip_totsect.iloc[:, [0, 1, 2, 5]]

# Paso 4: Combina por columnas
# Realiza la unión asegurando que las columnas [0, 1, 2] coincidan
tabla_share_sect = pd.merge(
    participsect_subset,
    participtotsect_subset,
    on=["Año", "Sector", "No_sector"],  # Especifica las claves para la unión
    how="inner"  # Usa "inner" para incluir solo coincidencias exactas
)

# Paso 5: Ordena por año y sector
tabla_share_sect = tabla_share_sect.sort_values(by=["Año", "No_sector"]).reset_index(drop=True)

# Paso 6: Cambia nombres de columnas
tabla_share_sect.columns = [
    "Año", "No_sector", "Sector", "Aguascalientes", "Baja California",
    "Baja California Sur", "Campeche", "Coahuila", "Colima", "Chiapas",
    "Chihuahua", "CDMX", "Durango", "Guanajuato", "Guerrero", "Hidalgo",
    "Jalisco", "Mexico", "Michoacan", "Morelos", "Nayarit", "Nuevo Leon",
    "Oaxaca", "Puebla", "Queretaro", "Quintana Roo", "San Luis Potosi",
    "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz",
    "Yucatan", "Zacatecas", "Total sector"
]

tabla_share_sect.to_excel("./python/Participacion sectorial.xlsx", index=False)


######################################################  
#############
#############               2a PARTE
#############     COEFICIENTE DE LOCALIZACION
#############     USANDO LA LIBRERIA REAT
######################################################  
 

# Renombrar las columnas del DataFrame
matriz.columns = [
    "Año", "No_sector", "Sector", "Aguascalientes", "Baja California", 
    "Baja California Sur", "Campeche", "Coahuila", "Colima", "Chiapas",
    "Chihuahua", "CDMX", "Durango", "Guanajuato", "Guerrero", "Hidalgo",
    "Jalisco", "Mexico", "Michoacan", "Morelos", "Nayarit", "Nuevo Leon",
    "Oaxaca", "Puebla", "Queretaro", "Quintana Roo", "San Luis Potosi",
    "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz",
    "Yucatan", "Zacatecas", "Total_sectorial"
]

# Define la función locq (adaptar según su lógica)
def locq(local_values, total_local, global_values, total_global):
    # Implementa la lógica de locq aquí. Esto es un placeholder:
    return (local_values / total_local) / (global_values / total_global)

# Para 2018
coeflocal2018 = []

for i in range(3, 35):  # Columnas 4 a 35 (índices 3 a 34)
    coefloc = locq(
        matriz.iloc[1:20, i].to_numpy(),  # Valores locales
        matriz.iloc[0, i],               # Total local
        matriz.iloc[1:20, 35].to_numpy(),# Valores globales
        matriz.iloc[0, 35]               # Total global
    )
    coeflocal2018.append(coefloc)

# Convertir coeflocal2018 en un DataFrame
coeflocal2018 = pd.DataFrame(np.column_stack(coeflocal2018))
coeflocal2018.columns = matriz.columns[3:35]  # Nombres de columnas (estados)
coeflocal2018.index = matriz.iloc[1:20, 1].to_list()  # Nombres de renglones (sectores)

import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages

def graficar_coeflocal(df, output_pdf=None):
    """
    Genera un gráfico de barras por cada columna del DataFrame.
    Incluye una línea roja en el valor 1.
    
    Parámetros:
    - df: DataFrame con las columnas a graficar y el índice como etiquetas.
    - output_pdf: Ruta del archivo PDF donde guardar las gráficas. Si es None, se muestran en pantalla.
    """
    # Si se proporciona una ruta para el PDF, lo abrimos
    if output_pdf:
        pdf = PdfPages(output_pdf)
    
    for col in df.columns:
        plt.figure(figsize=(10, 6))
        plt.bar(df.index, df[col], color='skyblue', alpha=0.8)
        plt.axhline(1, color='red', linestyle='--', linewidth=2)  # Línea roja en el valor 1
        plt.title(f"Gráfico de {col}", fontsize=14)
        plt.xlabel("Sectores", fontsize=12)
        plt.ylabel("Valor", fontsize=12)
        plt.xticks(df.index, rotation=45, ha='right', fontsize=10)
        plt.tight_layout()

        # Guardar en PDF o mostrar en pantalla
        if output_pdf:
            pdf.savefig()  # Guardar la figura en el PDF
            plt.close()    # Cierra la figura para evitar sobrecargar memoria
        else:
            plt.show()
    
    # Cierra el PDF si fue utilizado
    if output_pdf:
        pdf.close()
        print(f"Gráficos guardados en {output_pdf}")

# Uso con coeflocal2018
graficar_coeflocal(coeflocal2018, output_pdf="./python/coeflocal2018.pdf")  # Guardar en PDF
# graficar_coeflocal(coeflocal2018)  # Mostrar en pantalla


# Para 2013
coeflocal2013 = []

for i in range(3, 35):  # Columnas 4 a 35 (índices 3 a 34)
    coefloc = locq(
        matriz.iloc[21:40, i].to_numpy(),  # Valores locales
        matriz.iloc[20, i],               # Total local
        matriz.iloc[21:40, 35].to_numpy(),# Valores globales
        matriz.iloc[20, 35]               # Total global
    )
    coeflocal2013.append(coefloc)

# Convertir coeflocal2013 en un DataFrame
coeflocal2013 = pd.DataFrame(np.column_stack(coeflocal2013))
coeflocal2013.columns = matriz.columns[3:35]  # Nombres de columnas (estados)
coeflocal2013.index = matriz.iloc[21:40, 1].to_list()  # Nombres de renglones (sectores)


# Uso con coeflocal2013
graficar_coeflocal(coeflocal2013, output_pdf="./python/coeflocal2013.pdf")  # Guardar en PDF
# graficar_coeflocal(coeflocal2013)  # Mostrar en pantalla


# Combina las matrices por filas
coeflocal = pd.concat([coeflocal2018, coeflocal2013], axis=0)

# Agrega nombres de renglón secuenciales
coeflocal.index = range(1, len(coeflocal) + 1)

# Agrega las columnas Año, No_sector y Sector
coeflocal["Año"] = matriz.loc[1:19, "Año"].to_list() + matriz.loc[21:39, "Año"].to_list()
coeflocal["No_sector"] = matriz.loc[1:19, "No_sector"].to_list() + matriz.loc[21:39, "No_sector"].to_list()
coeflocal["Sector"] = matriz.loc[1:19, "Sector"].to_list() + matriz.loc[21:39, "Sector"].to_list()

# Cambia el orden de las columnas
cols_order = ["Año", "No_sector", "Sector"] + coeflocal.columns[:-3].to_list()
coeflocal = coeflocal[cols_order]
 
