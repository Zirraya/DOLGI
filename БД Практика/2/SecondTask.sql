
-- Удаление таблиц (если они существуют)
DROP TABLE IF EXISTS GoodsCheck;
DROP TABLE IF EXISTS FlowersAndBouquet;
DROP TABLE IF EXISTS Goods;
DROP TABLE IF EXISTS Account;
DROP TABLE IF EXISTS Bouquet;
DROP TABLE IF EXISTS Flowers;
DROP TABLE IF EXISTS AnotherGoods;
DROP TABLE IF EXISTS TypeGoods;
DROP TABLE IF EXISTS Client;


CREATE TABLE Client (
    Id SERIAL PRIMARY KEY NOT NULL,
    Name VARCHAR(64) NOT NULL,
    LastName VARCHAR(64) NOT NULL,
    Otchestvo VARCHAR(64), 
    PhoneNumber VARCHAR(20) NOT NULL,
    NumberPurchase INTEGER DEFAULT 0
);

CREATE TABLE Flowers (
    Id SERIAL PRIMARY KEY NOT NULL, 
    Species VARCHAR(64) NOT NULL,        -- Вид
    Name VARCHAR(64) NOT NULL,           -- Название
    LatName VARCHAR(64) NOT NULL,        -- Латинское название
    Country VARCHAR(64) NOT NULL         -- Страна происхождения
);

CREATE TABLE Bouquet (
    Id SERIAL PRIMARY KEY NOT NULL,
    Name VARCHAR(64) NOT NULL,           -- Название
    Quantity INTEGER NOT NULL,           -- Количество цветов
    Structure VARCHAR(64) NOT NULL,      -- Состав
    Price INTEGER NOT NULL               -- Цена
);

CREATE TABLE TypeGoods (
    IdTypeGoods SERIAL PRIMARY KEY NOT NULL,
    NameType VARCHAR(64) NOT NULL        -- Название типа
);

CREATE TABLE AnotherGoods (
    Id SERIAL PRIMARY KEY NOT NULL,
    Name VARCHAR(64) NOT NULL            -- Название
);

-- Связующие таблицы
CREATE TABLE FlowersAndBouquet (
    IdFlower INTEGER NOT NULL,
    IdBouquet INTEGER NOT NULL,
    PRIMARY KEY (IdFlower, IdBouquet),
	
    FOREIGN KEY (IdFlower) REFERENCES Flowers(Id) ON DELETE CASCADE,
    FOREIGN KEY (IdBouquet) REFERENCES Bouquet(Id) ON DELETE CASCADE
);

CREATE TABLE Goods (
    IdAnotherGoods INTEGER NOT NULL,
    IdType INTEGER NOT NULL,
	IdBouquet INTEGER UNIQUE, -- Добавлено поле для связи с букетом
    PRIMARY KEY (IdAnotherGoods, IdType),

	FOREIGN KEY (IdBouquet) REFERENCES Bouquet(Id) ON DELETE CASCADE,
    FOREIGN KEY (IdAnotherGoods) REFERENCES AnotherGoods(Id) ON DELETE CASCADE,
    FOREIGN KEY (IdType) REFERENCES TypeGoods(IdTypeGoods) ON DELETE CASCADE
);

CREATE TABLE Account (
    Id SERIAL PRIMARY KEY NOT NULL,
    IdClient INTEGER NOT NULL,
    Status VARCHAR(64) NOT NULL,
    DatePurchase DATE NOT NULL,
    Summ DECIMAL(10,2) NOT NULL,           -- Общая сумма
    ProcentDiscount DECIMAL(5,2) DEFAULT 0.00, -- Процент скидки
    SummAll DECIMAL(10,2) NOT NULL,        -- Итоговая сумма
	
    FOREIGN KEY (IdClient) REFERENCES Client(Id) ON DELETE CASCADE
);

CREATE TABLE GoodsCheck (
    IdCheck INTEGER NOT NULL,
    IdAnotherGoods INTEGER NOT NULL,  -- Добавлено объявление столбца
    IdFlower INTEGER NULL,
    Price DECIMAL(10,2) NOT NULL,
    PRIMARY KEY (IdCheck, IdAnotherGoods),
	
    FOREIGN KEY (IdCheck) REFERENCES Account(Id) ON DELETE CASCADE,
    FOREIGN KEY (IdAnotherGoods) REFERENCES AnotherGoods(Id) ON DELETE CASCADE
);

-- Запросы
-- Инсерты
 INSERT INTO Client (Name, LastName, Otchestvo, PhoneNumber, NumberPurchase) VALUES
('Иван', 'Иванов', 'Иванович', '+7(999)123-45-67', 10),
('Мария', 'Петрова', 'Сергеевна', '+7(912)345-67-89', 25),
('Алексей', 'Сидоров', 'Алексеевич', '+7(905)678-90-12', 5),
('Ольга', 'Смирнова', 'Ивановна', '+7(916)234-56-78', 15),
('Дмитрий', 'Кузнецов', 'Дмитриевич', '+7(903)456-78-90', 30),
('Елена', 'Попова', 'Владимировна', '+7(925)567-89-01', 8),
('Сергей', 'Васильев', NULL, '+7(926)678-90-12', 12),
('Анна', 'Павлова', 'Андреевна', '+7(927)789-01-23', 20),
('Андрей', 'Семенов', 'Петрович', '+7(928)890-12-34', 3),
('Наталья', 'Голубева', NULL, '+7(929)901-23-45', 18);

-- 
-- Делиты

DELETE FROM anothergoods ()



