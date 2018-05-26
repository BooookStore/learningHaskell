-- データ型
data Car = SUV | Van | Wagon deriving (Show)

-- これが型クラス
class Status a where
    run :: a -> String
    stop :: a -> String

-- Carをインスタンス化
instance Status Car where
    run SUV = "SUV Run." -- これが型クラス関数
    run Van = "Van Run."
    run Wagon = "Wagon Run."
    stop SUV = "SUV Stop."
    stop Van = "Van Stop."
    stop Wagon = "Wagon Stop."

-- 飛行機を表すデータ型
data Airplane = PropellerAircaft | JetAircraft

-- サブクラスを定義
class (Status s) => FlyStatus s where
    fly :: s -> String

instance Status Airplane where
    run PropellerAircaft = "PropellerAircaft run."
    run JetAircraft = "JetAircraft run."
    stop PropellerAircaft = "PropellerAircaft stop."
    stop JetAircraft = "JetAircraft stop."

instance FlyStatus Airplane where
    fly PropellerAircaft = "PropellerAircaft fly."
    fly JetAircraft = "JetAircraft fly."

instance (Show m) => Status (Maybe m) where
    run (Just x) = show x ++ " run"
    run Nothing = "Nothing run?"
    stop (Just x) = show x ++ " stop"
    stop Nothing = "Nothing stop?"