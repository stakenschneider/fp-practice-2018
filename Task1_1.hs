module Task1_1 where

    import Todo(todo)
    
    data Operator = Plus | Min | Mult deriving (Show, Eq)
    
    data Term = IntConstant{ intValue :: Int }           -- числовая константа
                | Variable{ varName :: String }          -- переменная
                | BinaryTerm{lhv :: Term, op :: Operator, rhv :: Term } -- бинарная операция
                deriving(Show,Eq)
    
    -- Для бинарных операций необходима не только реализация, но и адекватные
    -- ассоциативность и приоритет
    (|+|) :: Term -> Term -> Term
    (|+|) l r = BinaryTerm l Plus r
    infixl 6 |+|

    (|-|) :: Term -> Term -> Term
    (|-|) l r = BinaryTerm l Min r
    infixl 6 |-|

    (|*|) :: Term -> Term -> Term
    (|*|) l r = BinaryTerm l Mult r
    infixl 7 |*|
    
    
    -- Заменить переменную `varName` на `replacement`
    -- во всём выражении `expression`
    replaceVar :: String -> Term -> Term -> Term
    replaceVar varName replacement expression = case (expression) of 
        Variable variable | variable == varName -> replacement
                          | variable /= varName -> expression
        BinaryTerm lhv op rhv -> BinaryTerm (replaceVar varName replacement lhv) op (replaceVar varName replacement rhv)
        _ -> expression
    
    
    -- Посчитать значение выражения `Term`
    -- если оно состоит только из констант
    evaluate :: Term -> Term
    evaluate expression = case expression of
        BinaryTerm lhv operator rhv ->
            case (evaluate(lhv), operator, evaluate(rhv)) of 
                (IntConstant lhv, op, IntConstant rhv) | op == Plus -> IntConstant (lhv + rhv)
                                                       | op == Min -> IntConstant (lhv - rhv)
                                                       | op == Mult -> IntConstant (lhv * rhv)
                (IntConstant 0, Plus,  rhv) -> rhv
                (IntConstant 1, Mult,  rhv) -> rhv
                (IntConstant 0, Mult,  rhv) -> IntConstant 0
                (lhv, Plus, IntConstant 0) -> lhv
                (lhv, Min,  IntConstant 0) -> lhv
                (lhv, Mult,  IntConstant 1) -> lhv
                (lhv, Mult,  IntConstant 0) -> IntConstant 0
                _ -> BinaryTerm lhv operator rhv
        _ -> expression
    