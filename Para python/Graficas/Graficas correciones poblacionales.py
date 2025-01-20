import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from matplotlib.backends.backend_pdf import PdfPages

# Cargar datos de proyecciones poblacionales
proyeccion = pd.read_excel("Proyecciones poblacionales 2024_2035.xlsx", sheet_name="Sheet1")

# Convertir columnas de tipo texto a categórico
proyeccion = proyeccion.astype({col: "category" for col in proyeccion.select_dtypes("object").columns})

# Cambiar formato a largo
proy_long = pd.melt(
    proyeccion,
    id_vars=["CV_ENT", "Entidad"],
    value_vars=[col for col in proyeccion.columns if "Hombres" in col or "Mujeres" in col or  "Total" in col],
    var_name="Año_sexo",
    value_name="Poblacion",
)

# Separar columna Año_sexo en Sexo y Año
proy_long[["Sexo", "Año"]] = proy_long["Año_sexo"].str.split("_", expand=True)

# Convertir columnas relevantes a categóricas
proy_long = proy_long.astype({"Sexo": "category", "Año": "category", "Entidad": "category", "CV_ENT": "category"})

# Agrupar datos por estado, año y sexo
proy_estatal = (
    proy_long.groupby(["CV_ENT", "Entidad", "Año", "Sexo"], observed=False)
    .agg(Pob_Est=("Poblacion", "sum"), Pob_prom=("Poblacion", "mean"), desvstd=("Poblacion", "std"))
    .reset_index()
)

# Filtrar las filas para eliminar la categoría "Total" en "Sexo"
#proy_estatal = proy_estatal[proy_estatal["Sexo"] != "Total"].copy()

# Verificar las categorías de "Sexo"
#print(proy_estatal["Sexo"].unique())  # Debería mostrar solo "Hombre" y "Mujer"

# Generar identificador numérico para cada estado
proy_estatal["i"] = proy_estatal["CV_ENT"].cat.codes + 1

# Etiquetas de entidades
labels_entidad = [
    "Aguascalientes", "Baja California", "Baja California Sur", "Campeche",
    "Coahuila de Zaragoza", "Colima", "Chiapas", "Chihuahua", "Ciudad de México",
    "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "México",
    "Michoacán de Ocampo", "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla",
    "Querétaro", "Quintana Roo", "San Luis Potosí", "Sinaloa", "Sonora", "Tabasco",
    "Tamaulipas", "Tlaxcala", "Veracruz de Ignacio de la Llave", "Yucatán", "Zacatecas"
]

# Crear el archivo PDF
output_file = "ProyeccionPob_Todos_py.pdf"

# Lista de colores y marcadores ajustada al número de categorías en "Sexo"
palette = ["magenta", "blue", "green"]  # Dos colores
markers = ["o", "s", "d"]           # Dos marcadores

# Generar las gráficas para cada estado


with PdfPages(output_file) as pdf:
    for j in range(1, 33):  # 32 estados
        data = proy_estatal[proy_estatal["i"] == j]
        plt.figure(figsize=(11, 8))
        sns.lineplot(
            data=data,
            x="Año", y="Pob_Est", hue="Sexo", style="Sexo",
            markers=markers, palette=palette, errorbar=None
        )
        plt.title(f"Proyección de la población de 17 a 19 años \n{labels_entidad[j-1]}")
        plt.xlabel("Año")
        plt.ylabel("Población")
        plt.grid(False)
        plt.legend(title="Sexo")
        pdf.savefig()
        plt.close()

print(f"Gráficas guardadas en {output_file}")
