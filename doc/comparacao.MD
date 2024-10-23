# Comparação de código **Node.js** x **Haskell**

Vou pegar apenas alguns trechos de cada código para não deixar o arquivo tão grande.

## Iniciando um servidor

- Node.js

~~~javascript
const app = express();

// Middlewares
app.use(cors());
app.use(express.json());

// Usar as rotas
app.use('/api', routes);

// Porta da aplicação
const PORT = process.env.PORT || 8081;

// ...


app.listen(PORT, () => {
    console.log(`Servidor rodando na porta ${PORT}`);
});
~~~

- Haskell

~~~haskell
main :: IO ()
main = do
    initializeDb
    putStrLn "Servidor iniciado em http://localhost:3000"
    Scotty.scotty 3000 $ do
        Scotty.middleware logStdoutDev
        Scotty.middleware $ staticPolicy (addBase "static")
        routes
~~~

Não há diferenças tão grandes aqui, apenas que em node as funções do express são métodos de um objeto, já em haskell temos funções.

## Conexão ao banco de dados

- Node.js

~~~javascript
db.sequelize.sync({ force: false});

db.sequelize.authenticate();

//./config/db.js
const sequelize = new Sequelize(process.env.DATABASE, process.env.DATABASELOGIN, process.env.DATABASEPASSWORD, {
    host: 'localhost',
    dialect: 'postgres',
    logging: false,
});
~~~

- Haskell

~~~haskell
--.src/Main.hs
initializeDb :: IO ()
initializeDb = runSqlite "apiHaskell.db" $ do
    -- Executa as migrações
    void $ runMigration migrateAll

--./src/Controllers/ProductController.hs
liftIO $ runSqlite "apiHaskell.db" ProductServices.getAllProducts 
~~~

Deixei de fora uma parte do código responsável pelo tratamento de exceções e as impressões no console no código em node. No mais, não há diferenças gritantes, apenas na autenticação do banco de dados (nome do banco, login e senha), mas caso fosse utilizado postgreSQL em haskell também, as informações de autenticação seria necessária também.

## Rotas

- Node.js

~~~javascript
//./src/routes/product.js
const router = express.Router();

router.get('/getProducts', productController.getAllProducts);

router.get('/getProductById/:id', productController.getProductById);

router.post('/createProduct', productController.createProduct);

router.delete('/deleteProduct/:id', productController.deleteProduct);

router.patch('/updateProduct/:id', productController.updateProduct);

export default router;
~~~

- Haskell

~~~haskell
--./src/Routes/Routes.hs
routes :: ScottyM ()
routes = do
    get "/" $ do
        htmlFile <- liftIO $ readFile "./src/Public/Html/index.html"
        html $ mconcat [pack htmlFile]
    productsRoutes

--./src/Routes/ProductsRoutes.hs
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

A principal diferença entre os dois trechos é que o node.js usa funções de callback (função passada como parâmetro para outra função) para chamar os serviços (que estão na camada de controller) enquanto haskell simplesmente executa a função. Não há diferença no resultado final.

## Serviços

- Node.js

Um pouco complicado comparar aqui porque o código em node está mal estruturado (ainda estava aprendendo), então as validações, o controller e o service estão todos juntos no mesmo arquivo.

~~~javascript
export const createProduct = async (req, res, next) => {
    try {
        const { name, brand, price } = req.body;

        if(!name || name.length < 3) {
            return res.status(400).json({ message: 'Nome inválido'});
        }

        if(!brand) {
            return res.status(400).json({ message: 'Marca inválida' });
        }

        let formattedPrice = price.replace(/[^0-9.,]/g, '');
        formattedPrice = parseFloat(formattedPrice.replace(',', '.'));
        
        if (isNaN(formattedPrice) || formattedPrice < 0 || formattedPrice > 100000000) {
            return res.status(400).json({ message: 'Preço inválido' });
        }

        const newProduct = await Product.create({
            name: name,
            brand: brand,
            price: formattedPrice
        });

        return res.status(200).json(newProduct);
    } catch (error) {
        return res.status(500).json({ message: 'Ocoreu um erro interno', error });
    }
}
~~~

- Haskell

~~~haskell
createProduct :: MonadIO m => ProductData -> SqlPersistT m (Maybe (Key Products, Products))
createProduct p
    | not $ isValidName $ getName p = return Nothing
    | not $ isValidBrand $ getBrand p = return Nothing
    | not $ isValidPrice $ getPrice p = return Nothing
    | otherwise = do
        let newProduct = Products (getName p) (getBrand p) (roundTo 2 $ getPrice p)
        productId <- insert newProduct
        return $ Just (productId, newProduct)
~~~

Aqui o código em haskell está bem mais limpo, apesar do trecho em node estar mal estruturado, o uso da linguagem declarativa em haskell influencia bastante nesse aspecto.

## Outros pontos

Decompor e formar um arquivo json é bem mais fácil em node.js (devido a um midddleware do express.js) do que em haskell, muito provavelmente porque originalmente foi desenvolvido a partir da sintaxe de objetos em javascript.

- Decomposição em node

~~~javascript
//apenas um trecho do código

//./app.js
app.use(express.json());
//./controllers/productController.js
const { name, brand, price } = req.body;
~~~

- Decomposição em haskell

~~~haskell
--./src/Models/Product.hs
instance FromJSON ProductData where
    parseJSON = withObject "ProductData" $ \v -> ProductData
        <$> v .: "name"
        <*> v .: "brand"
        <*> v .: "price"

--./src/Controllers/ProductController.hs
p <- jsonData :: ActionM ProductData
~~~

- Criar json em node

Para retornar um produto não é necessário criar um json adicional, já que o próprio objeto em javascript já segue o padrão.

- Criar json em haskell

~~~haskell
--./src/Models/Product.hs
instance ToJSON Products where
    toJSON (Products n b p) =
        object
            [
                "name" .= n,
                "brand" .= b,
                "price" .= p
            ]

--./src/Controllers/ProductController.hs
json $ object ["products" .= products]
~~~

Outra grande diferença do haskell é a forte tipagem, enquanto javascript segue o caminho contrário.

Obviamente a diferença entre uma linguagem declarativa (haskell) e uma imperativa (javascript) é grande. O uso de IO com o banco de dados acaba afetando um pouco essa característica do haskell, mas a maneira como o código é escrito continua bem diferente.

# Consideração final

Apesar das diferenças, os códigos possuem uma certa similaridade, sendo completamente possível comparar ambos mesmo sem grande conhecimento em uma das duas linguagens.