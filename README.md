# qualityAssessment
The goal of this app is to have some quality indicators to assess the performance of a business.
## Tabs
### Dirección 
The first tab gives us a first glance of the business. Where are the branches or the contracts, Who are our suppliers, the annual turnover, the number of clients and the number of employees.
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
