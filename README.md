# Produção personalizada

## Aluno: Ivan Mendes Martignago

## Curso: Sistemas de Informação

## Sumário



- [Produção personalizada](#produção-personalizada)
  - [Aluno: Ivan Mendes Martignago](#aluno-ivan-mendes-martignago)
  - [Curso: Sistemas de Informação](#curso-sistemas-de-informação)
  - [Sumário](#sumário)
  - [Tema](#tema)
  - [Como rodar](#como-rodar)
  - [Endpoints](#endpoints)
  - [Processo de desenvolvimento](#processo-de-desenvolvimento)
    - [***Criação da estrutura de dados para armazenar o tipo Produto***](#criação-da-estrutura-de-dados-para-armazenar-o-tipo-produto)
    - [***Configuração do banco de dados***](#configuração-do-banco-de-dados)
    - [***Criação do Service de Produto***](#criação-do-service-de-produto)
    - [***Configuração do Web Scooty***](#configuração-do-web-scooty)
    - [***Criação do controller de Produto***](#criação-do-controller-de-produto)
    - [***Documentação da API***](#documentação-da-api)
    - [***Escopo pretendido **x** Escopo realizado***](#escopo-pretendido-x-escopo-realizado)
    - [***Conteúdos utilizados que não foram abordados em aula***](#conteúdos-utilizados-que-não-foram-abordados-em-aula)
  - [Comparação de código com o projeto anterior](#comparação-de-código-com-o-projeto-anterior)
  - [Considerações finais](#considerações-finais)
  - [Referências](#referências)

## Tema

O Objetivo do trabalho é o desenvolvimento de uma API Rest em Haskell para gerenciamento de produtos (CRUD). Foram utilizados Scotty para lidar com as requisições e persistent para acessar o banco de dados (SqLite). O projeto levou como base uma outra API no mesmo modelo que eu desenvolvi com Node.JS.

## Como rodar

Use ```cabal update && cabal build && cabal run -v0``` no terminal dentro do diretório principal.
Necessário ter ```ghc```, ```cabal``` e ```sqlite3``` instalados pelo menos (talvez tenha mais dependências, o container está configurado para rodar sem configuração adicional).

## Endpoints

Acesse ```localhost:3000/``` para ver a documentação dos endpoints da api. [Também pode clicar aqui](/doc/README.md).

## Processo de desenvolvimento

### ***Criação da estrutura de dados para armazenar o tipo Produto***

Como sugerido pela professora, implementei primeiro  Produto em uma estrutura de dados do tipo ```data```, deixando a implementação no banco de dados para depois. No fim, acabei reutilizando essa implementação mais para frente no projeto.
```./src/Entities/Product.hs```

~~~haskell
module Entities.Product where

data Product = Product
    {
    , name      :: String
    , brand     :: String
    , price     :: Float
    } deriving (Show, Eq)

getName :: Product -> String
getName product = name product

getBrand :: Product -> String
getBrand product = brand product

getPrice :: Product -> Float
getPrice product = price product
~~~

As funções get foram criadas com o objetivo de facilitar o acesso aos atributos da estrutura.

### ***Configuração do banco de dados***

Aqui vieram as primeiras dores de cabeça do projeto. Inicialmente minha ideia era usar postgreSQL ou MySql, porém, após gastar algumas horas tentando configurar o ```persistent``` com esses bancos de dados, acabei desistindo e optei por usar SQLite, pois no repositório da biblioteca haviam instruções de como configurar. Foi nessa etapa que comecei a usar o gerenciador de dependências ```cabal```, o que auxiliou muito no desenvolvimento alternado entre o codespaces e localmente.

Nessa etapa a estrutura de dados responsável por armazenar o Produto foi alterado a fim de trabalhar com o banco de dados.
```./src/Models/Product.hs```

~~~haskell
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Products
    name String
    brand String
    price Double
    deriving Show
|]
~~~

```./src/Main.hs```

~~~haskell
main :: IO ()
main = runSqlite "apiHaskell.db" $ do
    -- Executa as migrações
    void $ runMigration migrateAll

    void $ insert $ Products "Smartphone" "Samsung" 1999.99
~~~

Basicamente o ```main``` está executando três funções:

- ```runSqlite "apiHaskell.db"```: Inicia o banco de dados, recebe uma ```String``` com o nome do banco (se o banco não existir cria um).
- ```void $ runMigration migrateAll```: Roda uma migração no banco, Basicamente se um dos campos de Product foi alterado, atualiza a tabela no banco de dados para incluir as novas colunas.
- ```void $ insert $ Products "Smartphone" "Samsung" 1999.99```: Insere um produto no banco.

### ***Criação do Service de Produto***

Decidi deixar model, service e controller separados para facilitar a modularização do código, permitindo testar os serviços de Produto individualmente antes de implementar o ```Scotty```. O service é basicamente responsável por executar as regras de negócio e realizar as operações no banco de dados. Não tive grandes problemas nessa etapa, apenas uma pequena dificuldade com o tipo de retorno (essa parte resolvi com auxílio da IA). Aqui reaproveitei a estrutura ```data``` criada anteriormente para passar como argumento para algumas funções. Criei também um arquivo ```./src/Util/Validators.hs``` com as validações dos campos.

Operações no banco de dados:
```.src/Services/ProductsServices.hs```

~~~haskell
getAllProducts :: MonadIO m => SqlPersistT m [(Key Products, Products)]
getAllProducts = do
    entities <- selectList [] []
    return $ map (\entity -> (entityKey entity, entityVal entity)) entities
~~~

Pega todos os produtos do banco, e realiza um map na entidade retornada para criar uma tupla chave:valor (chave é o id, valor são os campos de produto).

~~~haskell
getProductById :: MonadIO m => Int64 -> SqlPersistT m (Maybe Products)
getProductById productIdInt = do
    let productId = toSqlKey productIdInt :: Key Products
    get productId
~~~

Ideia parecida com o ```getAll```, mas retorna apenas um produto, selecionado pelo Id. ```let productId = toSqlKey productIdInt :: Key Products``` converte de int para o tipo aceito pela função ```get```.

~~~haskell
createProduct :: MonadIO m => ProductData -> SqlPersistT m (Maybe (Key Products, Products))
createProduct p
    | not $ isValidName $ getName p = return Nothing
    | not $ isValidBrand $ getBrand p = return Nothing
    | not $ isValidPrice $ getPrice p = return Nothing
    | otherwise = do
        let newProduct = Products (getName p) (getBrand p) (getPrice p)
        productId <- insert newProduct
        return $ Just (productId, newProduct)
~~~

Aqui a estrutura ```data``` foi reaproveitada, passada como parâmetro na função. Faz as validações nos campos, se falhar em alguma, retorna ```Nothing```, se não, cria o produto e retorna ele.

~~~haskell
deleteProduct :: MonadIO m => Int64 -> SqlPersistT m Bool
deleteProduct productIdInt = do
    let productId = toSqlKey productIdInt :: Key Products
    maybeProduct <- get productId
    case maybeProduct of
        Just _ -> do                        
            delete productId
            return True
        Nothing -> return False
~~~

Busca o produto no banco pelo id, se não achar nada, retorna ```False```, caso contrário, deleta o produto e retorna ```True```.

~~~haskell
updateProductName :: MonadIO m => Int64 -> String -> SqlPersistT m (Maybe (Key Products, Products))
updateProductName productIdInt newName = do
    let productId = toSqlKey productIdInt :: Key Products
    maybeProduct <- get productId
    case maybeProduct of
        Just _ -> do
            if not (isValidName newName)
                then return Nothing
                else do
                    updatedProduct <- updateGet productId [ProductsName =. newName]
                    return $ Just (productId, updatedProduct)
        Nothing -> return Nothing
~~~

Recebe o id e o campo a ser atualizado, busca o produto pelo id e, caso não achar, retorna ```Nothing```. Se achar, faz a validação do campo e atualiza o produto se for válido. Retorna o produto atualizado ou ```Nothing``` se a validação falhar (na implementação do controller eu alterei isso para ter retornos diferentes dependendo do momento que falhar).

As Outras funções de ```update``` seguem a mesma lógica, só alterando o campo a ser validado.

Funções para teste do banco:

~~~haskell
-- Pega todos os produtos
      products <- getAllProducts
      liftIO $ print (products :: [(Key Products, Products)])
  -- Pega um produto pelo id
      products <- getProductById 2
      liftIO $ print (products :: Maybe Products)
  -- Cria um novo produto
      let newProduct = ProductData "Televisão" "Sony" 12000.00
      products <- createProduct newProduct
      liftIO $ print products
  -- Deleta um produto
      deleted <- deleteProduct 3
      if deleted
        then liftIO $ putStrLn "Produto deletado com sucesso."
        else liftIO $ putStrLn "Nenhum produto foi encontrado para deletar."
  -- Atualiza o campo name de um produto
      products <- updateProductName 2 "Computador"
      liftIO $ print products
  -- Atualiza o campo brand de um produto
      products <- updateProductBrand 2 "Dell"
      liftIO $ print products
  -- Atualiza o campo price de um produto
      products <- updateProductPrice 2 3000.00
      liftIO $ print products
~~~

```./src/Util/Validators.hs```

~~~haskell
module Util.Validators where

import Data.Char (isAlpha)

isValidName :: String -> Bool
isValidName name = not $ null name && all isAlpha name && (length name) > 3

isValidBrand :: String -> Bool
isValidBrand brand = not $ null brand && all isAlpha brand

isValidPrice :: Double -> Bool
isValidPrice price = price > 0
~~~

Atentar que as validações de nome e marca estão incorretas, foram corrigidas na etapa de controller (não tinha percebido antes).

### ***Configuração do Web Scooty***

A configuração do servidor web ocorreu sem grandes problemas, separei os arquivos de rotas do ```Main``` afim de manter o código organizado. O arquivo de rotas importado ```Routes.hs``` importa as rotas de cada model (fazia sentido levando em conta o escopo que pretendia para o projeto no início, falarei mais em [Escopo pretendido x Escopo realizado](#escopo-pretendido-x-escopo-realizado)).

```./src/Main.hs```

~~~haskell
main :: IO ()
main = do
    putStrLn "Servidor iniciado em http://localhost:3000"
    Scotty.scotty 3000 $ do
        Scotty.middleware logStdoutDev
        routes
~~~

O servidor roda na rota ```3000```, o middleware serve para debug, printando um log das requisições no terminal.

```./src/Routes/ProductsRoutes.hs```

~~~haskell
productsRoutes :: ScottyM ()
productsRoutes = do
    get "/products" $ html "Nothing here :D"
~~~

Nessa etapa do desenvolvimento não tinha implementado os controllers ainda, então a rota de produtos estava vazia.

```./src/Routes/Routes.hs```

~~~haskell
routes :: ScottyM ()
routes = do
    get "/" $ html "Hello, world!"
    get "/about" $ html "About page"
    productsRoutes
~~~

Chama ```productRoutes``` para importar as rotas de produtos.

### ***Criação do controller de Produto***

A maior etapa no desenvolvimento no projeto, onde corrigi e atualizei o código das etapas anteriores e implementei os controllers e rotas de produtos. O controller é responsável por tratar as requisições e oferecer retorno apropriado.

```./src/Models/Product.hs```

~~~haskell
instance ToJSON Products where
    toJSON (Products n b p) =
        object
            [
                "name" .= n,
                "brand" .= b,
                "price" .= p
            ]

instance FromJSON ProductData where
    parseJSON = withObject "ProductData" $ \v -> ProductData
        <$> v .: "name"
        <*> v .: "brand"
        <*> v .: "price"
~~~

Aqui usei ```Data.Aeson``` para as conversões de ```json``` para ```ProducData``` (```ProductData``` é o mesmo tipo ```data``` criado no início do projeto) usando uma instância de ```FromJSON``` e de ```Products``` para ```json```, usando uma instância de ```ToJson``` para o retorno para o cliente.

```./src/Routes/ProductsRoutes.hs```

~~~haskell
productsRoutes :: ScottyM ()
productsRoutes = do
    get "/products" $ getAllProducts
    get "/products/:id" $ getProductById
    post "/products" $ createProduct
    delete "/products/:id" $ deleteProduct
    patch "/products/:id/name" $ updateProductName
    patch "/products/:id/brand" $ updateProductBrand
    patch "/products/:id/price" $ updateProductPrice
~~~

Rotas para realizar o CRUD com produtos.

```./src/Util/Validators.hs```

~~~haskell
isValidName :: String -> Bool
isValidName name = not (null name) && all isAlpha name && length name > 3

isValidBrand :: String -> Bool
isValidBrand brand = not (null brand) && all isAlpha brand && (length brand) > 3
~~~

Resolvido o problema das funções de validação, que agoram retornam corretamente. Observe que apenas são aceitos caracteres alfanuméricos, condição removida na etapa [Documentação da API](#documentação-da-api)

```./src/Services/ProductsServices.hs```

~~~haskell
data UpdateError = InvalidRequest | ProductNotFound
    deriving (Show)

roundTo :: Int -> Double -> Double
roundTo n x = (fromInteger $ round $ x * (10^n)) / (10.0^^n)

-- ...

updateProductName :: MonadIO m => Int64 -> String -> SqlPersistT m (Either UpdateError (Key Products, Products))
updateProductName productIdInt newName = do
    let productId = toSqlKey productIdInt :: Key Products
    maybeProduct <- get productId
    case maybeProduct of
        Just _ -> do
            if not (isValidName newName)
                then return $ Left InvalidRequest
                else do
                    updatedProduct <- updateGet productId [ProductsName =. newName]
                    return $ Right (productId, updatedProduct)
        Nothing -> return $ Left ProductNotFound
~~~

Alterações no service:

- Adição da função ```roundTo``` para arredondar números, utilizada para garantir que números com apenas duas casas decimas sejam enviadas ao banco. [fonte](https://www.tutorialspoint.com/haskell-program-to-round-a-number-to-n-decimal-places)
- Adição do ```data``` ```updateError```, utilizado para retornar valores diferentes nas funções de ```update```, permitindo ao controller tratar corretamente problemas na execução.
- Atualização das funções de ```update``` usando ```either```, permitindo retornar os valores de ```updateError``` corretamente.

```./src/Controllers/ProductController.hs```

~~~haskell
data UpdateProductNameData = UpdateProductNameData
  { name :: String
  } deriving (Show, Generic)

instance FromJSON UpdateProductNameData
~~~

- ```data UpdateProductNameData```: Tipos ```data``` implementados para auxiliar na extração do ```body``` em caso de ```update```, permitindo extrair só o campo necessário.

~~~haskell
getAllProducts :: ActionM ()
getAllProducts = do
    products <- liftIO $ runSqlite "apiHaskell.db" ProductServices.getAllProducts  -- Provavelmente não é o modo mais perfomático, mas foi a maneira que eu achei
    status status200
    json $ object ["products" .= products]
~~~

Chamado por ```GET``` ```/products```, executa o serviço ```getAllProducts``` e retorna ```Status 200``` e um ```json``` com todos os produtos.

As outras funções seguem lógica parecida, retornando o código e mensagem apropriados.

### ***Documentação da API***

Nessa etapa criei um arquivo em markdown com a documentação dos endpoints da API. A base eu peguei de um modelo de outro projeto que estou trabalhando.

Configurei o endpoint ```/``` para exibir uma versão em ```html``` da documentação.

### ***Escopo pretendido **x** Escopo realizado***

Inicialmente pretendia implementar os models ```users```, ```cart``` e ```cartItems```, presentes no projeto original. Não implementei por falta de tempo.

Basicamente esses models junto com um mecanismo de autenticação eram responsáveis por uma operação simples de adicionar produtos ao carrinho de um usuário em específico. Também havia um sistema de permissões, usando um ```middleware``` nas rotas em que permissão adicional era necessária. Esse sistema era usado para garantir que apenas administradores tivessem permissão para gerenciar produtos.

### ***Conteúdos utilizados que não foram abordados em aula***

- Data: Usado para criar novos tipos de dados personalizados.
  - Deriving: Gera automaticamente instâncias de classes para o tipo de dados criado, como por exemplo, ```show``` permite converter o data para String facilmente.
- Aeson: Utilizado para trabalhar com arquivos ```json```, permitindo criar e decodificar.
- Maybe: Permite retornar um valor opcional.
  - ```Nothing```: Retorno vazio, normalmente usado em caso de erro.
  - ```Just a```: Retorno de um valor, normalmente usado em caso de sucesso.
- Either: Um pouco mais completo que ```maybe```, pois permite retornas mais valores.
  - ```Left```: Por convenção usado para retornar erros.
  - ```Right```: Por convenção usado para retornar cases de sucesso.
- Monad IO: Permite realizar ações que podem ter efeitos colaterais (como acessar o banco de dados) de uma maneira controlada.
- $: Usado para evitar parenteses, executando o que vem depois primeiro.

## Comparação de código com o projeto anterior

## Considerações finais

## Referências

Links e prompts usados para consulta durante o trabalho

- [Uso do data](https://chatgpt.com/share/671059ec-9c38-8010-8414-3a3928d7f8c4)
- [Configuração para usar sqlite no haskell](https://chatgpt.com/share/671301c5-a4f8-8010-b399-d6274a152f85)
- [Repositório da biblioteca persistent](https://github.com/yesodweb/persistent)
- [Explicação básica da biblioteca persistent](https://www.yesodweb.com/book/persistent)
- [Doc da biblioteca persistent](https://hackage.haskell.org/package/persistent-2.14.6.3/docs/Database-Persist-Class.html)
  - Mais precisamente é a página que explica o uso das funções de manipulação do banco de dados (crud)
- [Como construir uma API REST com haskell](https://dev.to/fabianveal/building-a-rest-api-with-haskell-2d54)
- [Prompt usado para construir a documentação](https://chatgpt.com/share/67181495-139c-8010-9ed1-b9be3bad79f9)
- [Aeson](https://hackage.haskell.org/package/aeson)
- [Maybe](https://hackage.haskell.org/package/base-4.20.0.1/docs/Data-Maybe.html)
- [Either](https://hackage.haskell.org/package/base-4.20.0.1/docs/Data-Either.html#v:Either)
- [Monad IO](https://chatgpt.com/share/67190291-7118-8010-bb99-23f0cef58a89)
- [Operador $](https://stackoverflow.com/questions/940382/what-is-the-difference-between-dot-and-dollar-sign)