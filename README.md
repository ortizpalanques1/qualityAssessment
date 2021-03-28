# qualityAssessment
The goal of this app is to have some quality indicators to assess the performance of a business.
## Tabs
### Dirección 
The first tab gives us a first glance of the business. Where are the branches or the contracts, Who are our suppliers, the annual turnover, the number of clients and the number of employees. 
### Satisfacción
Using the annual surveys, clients' and workers' satisfaction is measured and compared with their previous values. Aditionally, valueBoxes inform whether the annual goals have been reached.  
## MongoDB collections 
### clientes
1. Cliente
2. NombreExtendidoCliente
3. PK_Cliente
### contratosEspecificos
1. Cliente = PK_Cliente
2. Contrato_Nombre
3. Year
4. MontoConIVA
5. MontoSinIVA
6. FechaInicio
7. FechaFin
8. Contrato
9. PK_Contrato: Contrato + Year + Letter
10. FechaInicioPeriodo
11. FechaFinPeriodo
12. Observacion 
### itemsEncuestaTrabajadores 
The questions in the survey for the workers.
1. Dimension: The survey assess 7 different aspects of the relation of the workers with the company. This field classifies the question according to its aspect.  
2. Key: identifies each question. "P_" identifies the field as a question. The third character matches the Dimension. The last 1 or 2 characters identifies the question inside the dimension. 
3. Question: text of each question. 
### encuestaTrabajadoresReal 
1. The 41 questions. They use the Key of the previous collection as FK.
2. Year: the year of the survey. One each year. 

