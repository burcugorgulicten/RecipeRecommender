# RecipeRecommender
UBC CPSC 312 Project2 (Prolog)
Recipe Recommender helps the users to decide what to prepare
and it recommends recipes based on their input.
It uses the API Spoonacular and natural language processing to
get the user input to get the appropriate constraints and 
makes a request to create a file with the returned recipes.
It returns at most 3 results.

To load the files enter (it can take around 15 seconds):
``` bash
[main].
```

Then enter:
``` bash
q(R).
```

The constraints it accepts:
Time
Ingredients
Diet
Cuisine

It also accepts multiple sentence structures. 

Examples:
``` bash
Enter recipe query: What can I make with an egg in 15 minutes
R = ['Green Salad With Fresh Orange Juice Dressing', 'Cocoa Protein Pancakes', '5 Ingredient High Protein Pumpkin Pancakes'] .

Enter recipe query: Find me an Italian recipe with cheese
R = ['Pasta With Italian Sausage', 'Noodle Free Eggplant and Spinach Lasagna', 'Farmer\'s Market Wild Mushroom Risotto'] .

Enter recipe query: What gluten free recipes can I make with tomato and cucumber in less than half an hour?
R = ['Greek Side Salad'] .
```
with each of them writing the recipe informations in the file recipes.txt

It needs an apiKey for it to work. We have removed the key 
for privacy reasons. You can simply add the line 
```
apiKey(Key).
```
Where Key is the apiKey, in the file parse_recipe.pl
