# Produção personalizada paradigmas da programação

## Como rodar

Use ```cabal update && cabal build && cabal run -v0``` no terminal dentro do diretório principal.
Necessário ter ```ghc```, ```cabal``` e ```sqlite3``` instalados pelo menos (talvez tenha mais dependências, o container está configurado para rodar sem configuração adicional).

## Endpoints
Acesse ```localhost:3000/``` para ver a documentação dos endpoints da api. [Também pode clicar aqui](/doc/README.md)

## **TODO List**

- [x] Configuração do container
- [x] Elaboração da estrutura de dados de produto
- [x] Configuração para usar banco de dados
- [x] Criação dos services de produtos
- [x] Configuração do scotty
- [x] Criação do controller de produtos
- [ ] Documentação dos endpoints
- [ ] Documentação do projeto
- [ ] Comparação de código com o projeto anterior

## Referências

Links usados para consulta durante o trabalho

- [Uso do data](https://chatgpt.com/share/671059ec-9c38-8010-8414-3a3928d7f8c4)
- [Configuração para usar sqlite no haskell](https://chatgpt.com/share/671301c5-a4f8-8010-b399-d6274a152f85)
- [Repositório da biblioteca persistent](https://github.com/yesodweb/persistent)
- [Explicação básica da biblioteca persistent](https://www.yesodweb.com/book/persistent)
- [Doc da biblioteca persistent](https://hackage.haskell.org/package/persistent-2.14.6.3/docs/Database-Persist-Class.html)
  - Mais precisamente é a página que explica o uso das funções de manipulação do banco de dados (crud)
- [Como construir uma API REST com haskell](https://dev.to/fabianveal/building-a-rest-api-with-haskell-2d54)
- [Prompt usado para construir a documentação](https://chatgpt.com/share/67181495-139c-8010-9ed1-b9be3bad79f9)