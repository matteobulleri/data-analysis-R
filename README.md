# data-analysis-R

### Part 1 (Regression analysis)
In this part, you will perform tasks faced by a data scientist working for a medical research laboratory. You as the data scientist, obtain a dataset (“dataKidneyReg”) of historic data containing patient information, including the patients’ blood glucose level, which can help to determine whether a patient may develop (or have) certain illnesses. 

Thus, you are supposed to use machine learning to determine what factors may be linked to the blood glucose level in order to help doctors and patients to determine factors potentially influencing the glucose level and to adjust their recommendations and treatment options accordingly. 

In particular, your job is to make sense of the dataset from different perspectives and to build a regression model that attempts to explain the blood glucose level of the patients. From your employer you receive the dataset and the following short information about the dataset:

* Age: Age of the patient in years
* Blood_Pressure: in mm/Hg
* Gravity: Specific Gravity (i.e. Dangerousness)
* Sugar: Sugar level
* Red_Blood: Red blood cells present or not
* Bacteria: Bacteria present or not
*  Sodium: Sodium level in mEq/L
*  Potassium: Potassium level in mEq/L
*  Hemoglobin: in gms
*   Cell_Volume: Volume of the cell
*   White_Blood: White blood cell count
*   Red_BloodCount: Red blood cell count
*   Diabetes: Diabetes present or not
*   Pedal_Edema: Pedal edema present or not
*   Anemia: Anemia present or not
*   Glucose: Blood glucose level in mgs/dl 

The aim is to predict the blood glucose level of patients with the given features/ variables. The blood glucose level of patients is represented by the variable Glucose, which is the dependent variable, and the rest of the variables provide information to predict the value of the dependent variable

### Part 2 (Clustering)
In this exercise, you will have to work as a data scientist for an e-commerce platform that sells different personal products online. You obtain a dataset (“dataShopping”) containing information on customers and how they used the website of the e-commerce platform. Each row represents a session (=surfing on a webpage) of a unique customer on the website of the e-commerce platform. 

Your task will be to find similar types of users in terms of their usage behavior of the website and group them (via clustering). Moreover, you should try to understand what defines these different groups / what are their
characteristics.

The variables recorded are:

* Administrative_Duration: time (seconds) spent by a user on administrative pages of the website
* Informational_Duration: time (seconds) spent by a user on informational pages of the website
* ProductRelated_Duration: time (seconds) spent by a user on product-related pages of the website
* BounceRates: Percentage of visitors who enter the website from the same webpage (e.g. section of the website) as the current user and then leave ("bounce")
* SpecialDay: Closeness of the time a user visits the website to a specific special day (e.g. Mother’s Day, Christmas), with “1” meaning it was on a special day
* NewVisitor: A user is visiting the webpage for the first time or not
* Revenue: Indicates if the user’s session ended in an e-commerce transaction (= a purchase) or not

