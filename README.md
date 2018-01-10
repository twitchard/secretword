# Secret Word

*Secret Word* is an Alexa Skill, based off the logic-oriented word game ["Jotto"](https://en.wikipedia.org/wiki/Jotto), implemented in Purescript.

## Rules

When the game starts, a five-letter "secret word" is selected at random from a [corpus of common English words](src/Words.purs). The player's goal is to discover the secret word in as few turns as possible. Each turn, the player guesses a five-letter English word, and is told the total number of letters that the guess has in common with the secret word.

### Duplicate letters
Letters that are duplicated in both the player's guess and the secret word count multiple times towards the total, once for each duplication that is present in both words. The following table is informative.

| Word      | Word          | Total |
|-----------|---------------|-------|
| ru**mm**y | **m**ada**m** |   2   |
| a**pp**le | **p**roud     |   1   |
| ch**eer** | **eer**i*e*   |   3   |

For example, *rummy* and *madam* are reported to have 2 letters in common, because they both contain 2 m's. The words *apple* and *proud* are reported to have only 1 letter in common, because although p appears twice in apple, it only appears once in proud. Finally, the words *cheer* and *eerie* are reported to have 3 letters in common. Eerie has three e's, but cheer has only two, so e contributes 2 to the total. Both words also have an *r*, so the reported total is 3.


## Implementation

