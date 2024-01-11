# Blackjack Advisor & Analyzer


This project is a practical exploration of functional programming concepts, focusing on the Clojure programming language. The application serves as a blackjack advisor and analyzer, designed to aid players in making informed decisions during gameplay.

## Functionality
The program functions as a real-time advisor for players engaged in blackjack games. It leverages statistically calculated odds to offer insightful feedback, aiding players in deciding optimal moves during gameplay. The advisor provides valuable information, helping users make strategic decisions that enhance their overall gaming experience.

## Business logic

Player is prompted to enter the number of decks they want to play with, accepting values from 1 to 8, and defaults to 1 deck if the input is invalid or unspecified.

Then, player is again prompted to enter their beginning hand, which consists of two cards and the dealer's card as well.

The player will be recommended which move to make. Move decisions will be made using the provided blackjack cheat sheet to follow the basic strategy. By adhering to the basic strategy, the player has the opportunity to reduce the casino's advantage to around 0.5%.

Every move recommendation is accompanied by mathematically calculated odds of winning associated with that particular move.

The concept is that if a player follows the advice provided by the advisor, we cannot guarantee a win, but the likelihood of success is significantly increased. Over the long term, consistently playing in accordance with the provided guidance should position the player as a potential winner.
## Conclusion
Overall, this project has been enjoyable, and I've learned a lot about the Clojure language. Although certain sections of the code presented challenges when it came to organization and expansion, I actively worked towards maintaining concise functions and breaking down the code into more manageable segments. I acknowledge that there's still room for improvement in simplifying complex functions that involve numerous conditions. Adding a graphical user interface would be a great improvement, making it easier for users and enhancing their experience. Despite these challenges, I'm quite pleased with my performance, particularly given that this is my initial venture into Clojure projects.

