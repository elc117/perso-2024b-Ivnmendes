# Endpoints

## Produtos

- #### getAllProducts

- **Descrição:** Pega todos os produtos do sistema.
- **URL:** `/products`
- **Método:** `GET`
- **Request body:** Nenhum
- **Response: 200 Ok**

  ~~~json
    {
    "products": [
        {
        "brand": "Marca Exemplo",
        "name": "produto",
        "price": 80
        }   
        ]
    }
  ~~~

---

- #### getProductById

- **Descrição:** Pega um produto do sistema pelo id.
- **URL:** `/products/:id`
- **Método:** `GET`
- **Parâmetros da URL**
  - `id`: ID do produto pretendido.
- **Request body:** Nenhum
- **Response: 200 Ok**

  ~~~json
    {
        "products": {
            "brand": "Marca Exemplo",
            "name": "produto",
            "price": 80
        }
    }
  ~~~

- **Erros comuns**
  - `404 Nenhum produto encontrado com esse id`: Não existe produto cadastrado com esse id.

---

- #### createProduct

- **Descrição:** Cria um novo produto.
- **URL:** `/products`
- **Método:** `POST`
- **Request body:**

  ~~~json
    {
        "name": string,
        "brand": string,
        "price": double
    }
  ~~~

  - `name`: Deve ter entre 1 e 3 caracteres.
  - `brand`: Deve ter entre 1 e 3 caracteres.
  - `price`: Deve ser maior que 0.
- **Response: 201 Created**

  ~~~json
    {
        "products":[
            3,
            {
                "brand":"Marca Exemplo",
                "name":"Produto Exemplo",
                "price":99.99
            }
        ]
    }
  ~~~

- **Erros comuns**
  - `400 Um ou mais campos inválidos`: Body preenchido incorretamente.

---

- #### deleteProduct

- **Descrição:** Deleta um produto do sistema pelo id.
- **URL:** `/products/:id`
- **Método:** `DELETE`
- **Parâmetros da URL**
  - `id`: ID do produto pretendido.
- **Request body:** Nenhum
- **Response: 204 No content**

- **Erros comuns**
  - `404 Produto não encontrado`: Não existe produto cadastrado com esse id.

---

- #### updateProductName

- **Descrição:** Atualiza o nome de um produto existente.
- **URL:** `/products/:id/name`
- **Método:** `PATCH`
- **Parâmetros da URL**
  - `id`: ID do contribuidor pretendido.
- **Request body:**

  ~~~json
    {
        "name": string
    }
  ~~~

  - `name`: Deve ter entre 1 e 3 caracteres.

- **Response: 200 Ok**

  ~~~json
    {
        "products":[
            3,
            {
                "brand":"Marca Exemplo",
                "name":"Novo Nome",
                "price":99.99
            }
        ]
    }
  ~~~

- **Erros comuns**
  - `400 Nome inválido`: Body preenchido incorretamente.
  - `404 Produto não encontrado`: Não existe produto cadastrado com esse id.

---

- #### updateProductBrand

- **Descrição:** Atualiza a marca de um produto existente.
- **URL:** `/products/:id/brand`
- **Método:** `PATCH`
- **Parâmetros da URL**
  - `id`: ID do produto pretendido.
- **Request body:**

  ~~~json
    {
        "brand": string
    }
  ~~~

  - `age`: Deve ter entre 1 e 3 caracteres.

- **Response: 200 Ok**

  ~~~json
    {
        "products":[
            3,
            {
                "brand":"Marca Nova",
                "name":"Nome Exemplo",
                "price":99.99
            }
        ]
    }
  ~~~

- **Erros comuns**
  - `400 Marca inválida`: Body preenchido incorretamente.
  - `404 Produto não encontrado`: Não existe produto cadastrado com esse id.

---

- #### updateProductPrice

- **Descrição:** Atualiza o preço de um produto existente.
- **URL:** `/products/:id/price`
- **Método:** `PATCH`
- **Parâmetros da URL**
  - `id`: ID do produto pretendido.
- **Request body:**

  ~~~json
    {
        "price": double
    }
  ~~~

  - `graduation`: Deve ser maior que 0.

- **Response: 200 Ok**

  ~~~json
    {
        "products":[
            3,
            {
                "brand":"Marca Exemplo",
                "name":"Nome Exemplo",
                "price":199.99
            }
        ]
    }

  ~~~

- **Erros comuns**
  - `400 Preço inválido`: Body preenchido incorretamente.
  - `404 Produto não encontrado`: Não existe produto cadastrado com esse id.
