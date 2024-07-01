import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
type Student = (Int, String, String, Int)
type Lista = [Student]

adicionaEstudante::Lista->Student->Lista
adicionaEstudante ls val = val:ls

getId::Student->Int
getId (id, _, _, _) = id

buscaEstudante::Lista->Int->Student
buscaEstudante ls id = head $ filter (\x->getId x==id) ls

atualizaEstudante::Lista->Student->Lista
atualizaEstudante ls st = map (\x->if getId x == getId st then st else x) ls

main::IO()
main = menu []

menu::Lista->IO()
menu lista = do
        hSetBuffering stdout NoBuffering
        putStrLn ""
        imprimeMenu
        opcao<-getLine
        putStrLn ""
        case (read opcao::Int) of
            1->do
                est<-leEstudante
                putStrLn "\nEstudante adicionado"
                menu $ adicionaEstudante lista est
            2->do
                id<-leId
                putStrLn $ estudanteToString $ buscaEstudante lista id
                menu lista
            3->do
                est<-leEstudante
                putStrLn "\nEstudante atualizado"
                menu $ atualizaEstudante lista est
            4->do
                putStr (foldr ((++) . (++ "\n") . estudanteToString) "" lista)
                menu lista
            5->putStr ""
            _->do
                putStrLn "Opcao invalida, tente novamente"
                menu lista

imprimeMenu::IO()
imprimeMenu = putStr "1- Adicionar estudante\n2- Buscar estudante\n3- Atualizar dados de estudante\n4- Imprimir todos os estudantes\n5- Sair\nOpção: "

estudanteToString::Student->String
estudanteToString (id, nome, sobrenome, idade) =  show id ++ " - " ++ nome ++ " " ++ sobrenome ++" - Idade: " ++ show idade

leEstudante::IO Student
leEstudante = do
                putStr "Insira o id do estudante: "
                linha<-getLine
                let id = read linha ::Int
                putStr "Insira o nome do estudante: "
                nome<-getLine
                putStr "Insira o sobrenome do estudante: "
                sobrenome<-getLine
                idade<-getIdade
                return (id, nome, sobrenome, idade)

getIdade::IO Int
getIdade = do
            putStr "Insira a idade do estudante: "
            idade <- readLn::IO Int
            if idade<=0 then do {putStrLn "Idade invalida, tente novamente\n"; getIdade} else return idade

leId::IO Int
leId = do
        putStr "Insira o ID a ser buscado: "
        readLn::IO Int