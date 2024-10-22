# Comandos para testar os endpoints

Os comandos foram gerados pelo chatGPT com o seguinte promt:

"considere os seguintes endpoints:
/products : get retorna todos os produtos
/products:id : get retorna um produto
/products : post cria um produto modelo do body: { "name": string, "brand": string, "price": double }
/products/:id : delete deleta um produto
/products/:id/name : patch atualiza o nome modelo do body: { "name": string }
/products/:id/brand : patch atualiza a marca modelo do body: { "brand": string }
/products/:id/price : patch atualiza o preço modelo do body: { "price": double }

crie comandos curl pra esses endpoints, considere que estão hospedados em localhost:3000"

~~~~bash

#getAllProducts
curl -X GET http://localhost:3000/products

#getProductById
curl -X GET http://localhost:3000/products/1

#createProduct
curl -X POST http://localhost:3000/products \
     -H "Content-Type: application/json" \
     -d '{"name": "Produto Exemplo", "brand": "Marca Exemplo", "price": 99.99}'

#deleteProduct
curl -X DELETE http://localhost:3000/products/1 

#updateProductName
curl -X PATCH http://localhost:3000/products/1/name \
     -H "Content-Type: application/json" \
     -d '{"name": "Novo Nome"}'

#updateProductBrand
curl -X PATCH http://localhost:3000/products/1/brand \
     -H "Content-Type: application/json" \
     -d '{"brand": "Nova Marca"}'

#updateProductPrice
curl -X PATCH http://localhost:3000/products/1/price \
     -H "Content-Type: application/json" \
     -d '{"price": 79.999}'

~~~~

Se hospedar em um local diferente de localhost:3000, não esqueça de mudar no comando
