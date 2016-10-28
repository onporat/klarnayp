# Klarna's home exercise
Welcome to Klarna's home exercise! As you've no doubt been told, the aim of this exercise is to see how you actually write code in an optimal environment (or at least, an environment that is better than in an interview room on a machine you don't know while a couple of people are staring at you).

The exercise also aims to present you with a task that is reminiscent of the sort of work we actually do at Klarna, and so you will be building a web application.

The exercise shouldn't take overly long to complete, and your effort is certainly appreciated.

## The web app
You'll be implementing a "Yellow Pages" web application. The application consists of a form with a single input field that allows searching for people by their _name_, _age_, _phone number_, or any combination thereof. As there is only a single input field, the application must do its best to parse the search terms entered by its users. `John 33` should return all the people who are called John and are 33 years old, while `099373927 Smith` should attempt to find a person with the supplied phone number whose name contains "Smith".

Should the search yield no results, the user should be presented with the message: `No results, please review your search or try a different one`

If the search did yield results, they should be presented to the user. Each result shown must include the following:

* Name
* Age
* Phone
* Address
* Portrait picture (jpg)

### What we give you
Creating this from scratch could take quite a bit of time. Seeing how we realize your time is valuable, we'd like to help! Here is a list of things that we'll supply you with. Some are essential to actually complete the exercise and others are just meant to make your life easier:

* HTML/CSS for the search form and the results view. You can find these in the `assets` folder, though they will probably require some tweaks to suit your specific solution.
* A big heap of data. This data is where your application will get all it's information, and is essentially a very big JSON file containing information about people. This data set is expected to grow *significantly* over time. Surprisingly, you can find the data in the `data` folder.

### What we want from you
We're rooting for you, we really are. Have a look at this handy list of things that your exercise should strive to have:

* Readable code
* Proper treatment of errors and edge cases
* A backend
* Efficiency

It is important to remember that you should solve the exercise in a way that best suits your skill set. Are you unparalleled in your backend skills? Go for a minimal frontend. Are you a phenomenal frontend developer? Put the bulk of your logic in the frontend. Anything in between also flies. We're very open minded that way.

### How do I hand this in?
You have two options:

1. Host it somewhere online, give us the link
2. Send us instructions on how to spin up your solution locally

Either way, we obviously want your code so be sure to send that over.

## Questions?
Feel free to get in touch with us (just reply to the thread where you got this exercise) if things are unclear.

Good luck!
