module Animales where
import System.IO

data QuestionTree = Animal String | Question String QuestionTree QuestionTree
data Answer = Yes | No

instance Show QuestionTree where
    show (Animal name) = (if last name == 'a' then "una " else "un ") ++ name
    show (Question q _ _ ) = q

play :: QuestionTree -> IO QuestionTree
play root = do
    putStrLn "Piensa en un animal, voy a intentar adivinar"
    newRoot <- play' root
    playAgain <- ask "Queres jugar de nuevo?"
    case playAgain of
        Yes -> play newRoot
        No -> do
            putStrLn "Gracias por jugar."
            return newRoot

play' :: QuestionTree -> IO QuestionTree
play' animal@(Animal _) = do
    ans <- ask $ "Estas pensando en " ++ show animal ++ "?"
    case ans of
        Yes -> do
            putStrLn "Esta vez gano yo."
            return animal
        No -> do
            putStrLn "Me doy por vencido, tu ganas"
            getNewAnimal animal

play' question@(Question q s n) = do
    ans <- ask q
    case ans of
        Yes -> do
            newYes <- play' s
            return $ Question q newYes n
        No -> do
            newNo <- play' n
            return $ Question q s newNo

getNewAnimal :: QuestionTree -> IO QuestionTree
getNewAnimal animal = do
    putStrLn "Ayudame a mejorar!"
    putStrLn "En que animal estabas pensando?"
    name <- getLine
    let newAnimal = Animal name
    putStrLn "Decime una pregunta de SI para este animal"
    question <- getLine
    return $ Question question newAnimal animal

ask :: String -> IO Answer
ask s = do
    putStrLn $ s ++ " (s/n)"
    getAnswer

getAnswer :: IO Answer
getAnswer = do
    ans <- getChar
    putStrLn ""
    case ans of
       's' -> return Yes
       'n' -> return No
       _   -> putStrLn "Decime si o no: 's' o 'n'..." >> getAnswer
