![diabetes](https://user-images.githubusercontent.com/91103250/195938546-30462227-1cc9-4025-8005-eb6636e61ec0.jpg)
*Fonte: https://imo.com.br/diabeticos-apresentam-maior-risco-de-queda/*
# Análise Preditiva - Diabetes

Nesse repositório o objetivo será fazer uma análise preditiva com dados de pessoas que tiveram ou não diabetes usando um modelo de regressão logística. Com base nos sintomas apresentados, o modelo poderá fazer uma previsão aproximada do risco da pessoa ter ou não diabates. O objetivo do modelo é ter uma acurácia acima de 70% pelo menos.

### Introdução

Diabetes Mellitus (DM) é uma síndrome metabólica de origem múltipla, decorrente da falta de insulina e/ou da incapacidade de a insulina exercer adequadamente seus efeitos. A insulina é produzida pelo pâncreas e é responsável pela manutenção do metabolismo da glicose e a falta desse hormônio provoca déficit na metabolização da glicose e, conseqüentemente, diabetes. Caracteriza-se por altas taxas de açúcar no sangue (hiperglicemia) de forma permanente.

Tipos de Diabetes:

- Tipo 1: causada pela destruição das células produtoras de insulina, em decorrência de defeito do sistema imunológico em que os anticorpos atacam as células que produzem a insulina. Ocorre em cerca de 5 a 10% dos diabéticos.
- Tipo 2: resulta da resistência à insulina e de deficiência na secreção de insulina. Ocorre em cerca de 90% dos diabéticos.
- Diabetes Gestacional: é a diminuição da tolerância à glicose, diagnosticada pela primeira vez na gestação, podendo ou não persistir após o parto. Sua causa exata ainda não é conhecida.
- Outros tipos: são decorrentes de defeitos genéticos associados com outras doenças ou com o uso de medicamentos. Podem ser: defeitos genéticos da função da célula beta; defeitos genéticos na ação da insulina; doenças do pâncreas exócrino (pancreatite, neoplasia, hemocromatose, fibrose cística, etc.); induzidos por drogas ou produtos químicos (diuréticos, corticóides, betabloqueadores, contraceptivos, etc.).

*Fonte: https://bvsms.saude.gov.br/diabetes/*

### Dicionário de Dados

| Atributo  | Descrição | Métrica |
| ------------- | ------------- | ------------- |
| Age  | Idade  | 20-65  |
| Sex  | Gênero  | Male / Female  |
| Polyuria  | Urina constante  | Y / N  |
| Polydipsia  | Sede constante, mesmo após beber muita água  |  Y / N  |
| sudden weight loss  | Aparente perda de peso  | Y / N  |
| Polyphagia  | Comer em excesso devido a fome excessiva ou apetite elevado  | Y / N  |
| Genital thrush  | Candidíase genital  | Y / N  |
| visual blurring  | Confusão visual  | Y / N  |
| Itching  | Sensação de coceira no corpo constante  | Y / N  |
| Irritability  | Irritação  | Y / N  |
| delayed healing  | Demora para curar ferimentos na pele  | Y / N  |
| partial paresis  | Fraqueza muscular  | Y / N  |
| muscle stiness  | Atrofia muscular | Y / N  |
| Alopecia  | Queda de cabelo (Alopecia)  | Y / N  |
| Obesity  | Obesidade  | Y / N  |
| Class  | Classificação se é ou não diabético  | Positive / Negative  |

Informações sobre o dataset:

|  |  |
| ------------- | ------------- |
| Total de registros  | 520|
| Total de atributos  | 17 |

*Fonte: https://archive.ics.uci.edu/ml/datasets/Early+stage+diabetes+risk+prediction+dataset*
