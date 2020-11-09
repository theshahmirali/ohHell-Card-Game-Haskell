module Player (
    playCard,
    makeBid
)
where
{-
makeBid Strategy: My strategy to make bids is that first I check that if I am the last player then I obey the hook rule else I continue with my normal bid which is planned out in some way like this in "myMain", where it first checks that if I have more than 0 cards in my hand then I continue with myBid else I return 0 bid
 Where I first try to find the bigCards that are ranked after the Queen and I filter out those cards and return in a list, then from those cards I try to remove my trump cards so that when I bid I mistakenly don't duplicate a card. Then using this nonTrumpcard list I can place a Bid for cards gretaer than the Queen rank. 
Then I try to find all the Trump cards present in my hand and check if I have a trump card greater than eight I add that into myBid.
So when bidding I bid all nontrump cards that are greater than Queen + all the trump cards that are greater than the rank Eight. 
I also applied the hooke rule condition that if my player is the last player then I follow this rule.

playCard Strategy: So my startegy over here is that I first check if my player is the first person to play the card, then I set a strategy for that particular condition. If my player is in any other position to play the Card then I've set another strategy, which follows the reneg law, where players must follow the lead suit, unless and until we do not have a lead suit card in our hand, in this case I play with a different Strategy to check my trumpCards as well so that I dont waste them if they are already played by another player. Below I've explained in detail my strategies to try winning the game.
    First player to play the Card: If I am the first player to play the Card then my Strategy is simple that I take a risk and play the Highest card that I currently have in my hand. 
Decision for this approach: I chose this risky Strategy to basically when I by luck get a card of Rank Ace unless and until some other player beats the card early on using a trump card.
    Any other play position: If I am in any other position of playing the Card not being the first player than my strategy is to follow strictly the lead suit then I check the cards that are played already by other players in this Trick, if I by chance I have a card greater than the highest rank card so far in the lead suit then I play that card else I play the smallest ranked Card that I have in my hand.
Decision for this approach: I chose this approach ton basically calculate my chances of winning the game smartly, by comparing the ranks and following the lead suit, rather than just blindly playing any card.
Non lead suit Card: The strategy over here that in case I have no leas Suit Card in my hand then I check if I have a trumpCard present so I can play my trumpCard but keeping this also in mind that if any other player has played the trumpCard so I dont waste myTrump card there and I just play the lowest rank Card from my hand. But if the trumpCard is not played so far then I play the highest trumpCard that I currently have in my hand to try winning the round.
Decision for this approach: I chose this strategy so that I can save my trumpCards towards the end of the rounds where others have mostly played their trumpCards and now I can try and win using my trumpCards.

For the functions and how I implemented them using Functors,filters, typechecking and guards from the Lecture in to my Assignment following a Functional Programming style has been described below amd commented after each line of code.
-}

import OhTypes
import OhHell
import Data.List --imported to use the sort

-- | Play a card for the current trick.
-- If you are the "lead" player, you must follow the suit of the card that was led.
playCard :: PlayFunc
playCard _myPlayer myCards _allBids trumpCard _totalTrickCards currentTrickCards  
        | ((length firstPlayer) == 0) = strategy1 -- checks the length if first player or not then executes strategy1                                         
        | otherwise = strategy2                   -- else executes strategy2
    where 
        firstPlayer = fst <$> currentTrickCards  -- extracts only the Card from Trick list using a Functor 
        strategy1                                -- my strategy1
            | (length myCards > 0) = playCard1              -- checks for length of the hand to execute
            | otherwise = restPlayer                  -- else plays strategy for the rest of the poistions
        playCard1                                -- playCard1 strategy
            | (length myCards > 0) = playingOption1                  
            | otherwise = restPlayer                                     
        sortMyCards = sort myCards               -- sort the cards in my hand
        playingOption1 = last sortMyCards        -- and plays the biggest card in the sorted hand
        restPlayer 
            | ((length firstPlayer) == 0) = strategy1  -- checks if the player is first to play the card or not, if yes then it executes the above code again
            | otherwise = strategy2              -- else it executes my strategy2
        strategy2 
            | ((length sortLeadSuitMyCards) > 0) = playCard2 -- where startegy2 checks if I have a lead suit card and executes it then executes my playCard2
            | otherwise = passNotEmpty3          -- else it goes to the checkempty list condition
        extractCard = fst <$> currentTrickCards  -- extracts only Card from Trick using a Functor
        extractSuit (Card s _ ) = s              -- typeclass check to get the suit of the lead suit palyed
        getSuit = extractSuit <$> extractCard    -- using Functor to extract the suits
        getLeadSuit = last getSuit               -- getting the lead suit as it is the last card in the list 
        leadSuitMyCards = filter (\(Card s _) -> getLeadSuit == s ) myCards  -- uses filter to typecheck and find the leadsuit cards in my hand of cards currently and returns it in a list.
        sortLeadSuitMyCards = sort leadSuitMyCards -- sort the lead suit cards
        playCard2  
            | ((length sortTrickRank) > 0) = passNotEmpty                              -- checks if a card exists then it goes to the checkempty list condition for playoption2
            | otherwise = passNotEmpty2                                                -- else it goes to the checkempty list condition for playoption3
        filterTrickByLeadSuit  = filter (\(Card s _) -> getLeadSuit == s ) extractCard -- uses filter to typecheck and find the leadsuit cards in the trick currently and returns it in a list.
        extractTrickRank (Card _ r) = r                                                -- typeclass check to get the rank of the lead suit palyed
        getTrickRank = extractTrickRank <$> filterTrickByLeadSuit                      -- extracts the rank of the lead Suit
        sortTrickRank = sort getTrickRank                                              
        maxTrickRank = last sortTrickRank
        myCardGreatTrickCard = filter (\(Card _ rank) -> rank > maxTrickRank) leadSuitMyCards -- uses filter and checks whether my player has a card of higher rank in my hands compared to the Cards already played in Trick
        passNotEmpty                              
            | ((length myCardGreatTrickCard ) > 0) = playingOption2  -- checks if the list is not empty then if not empty only then it plays the card                               
            | otherwise = passNotEmpty2                              -- else it goes to the next check for empty list   
        playingOption2 = head myCardGreatTrickCard                   -- plays the high rank card following the suit 
        passNotEmpty2                             
            | ((length sortLeadSuitMyCards) > 0) = playingOption3    -- checks if the list is not empty, if not empty only then it plays the card 
            | otherwise =  passNotEmpty3                             -- else it goes to the next check for empty list  
        playingOption3 = head sortLeadSuitMyCards                    -- plays the safe option which is the smallest card         
        passNotEmpty3 
            | ((length sortLeadSuitMyCards) > 0) = nonLeadSuit    -- checks if the list is not empty, if not empty only then it executes the non lead suit part of the code
            | otherwise = head myCards                  -- else plays the head of my hand
        nonLeadSuit                                     -- the strategy for non lead suit cards 
            | (length myCards < 7) = playTrump          -- if the length of my cards is less than 7 then play the trump strategy
            | otherwise = playNormal                    -- else play the normal strategy
        trumpSuit (Card s _ ) = s                       -- typeclass check to get the rank of the trump suit
        getTrumpSuit = trumpSuit trumpCard              -- get the trump suit
        trumpMyCards = filter (\(Card s _) -> getTrumpSuit == s ) myCards  -- uses filter to typecheck and find the trump cards in my hand of cards currently and returns it in a list
        sortTrumpMyCards = sort trumpMyCards  
        playTrump    
            | (length trumpSuitPlayed > 0) = playSafe          -- checks whther the a trumpsuit card is played by a player before, then if played then we play safe
            | otherwise = playTrumpCard                        -- else we play a trump card
        trumpSuitPlayed = filter (\(Card s _) -> getTrumpSuit == s ) extractCard  -- uses filter and checks whether a trump card has been played or not by any player so far
        playSafe = head sortLeadSuitMyCards
        playTrumpCard = last sortTrumpMyCards                  -- plays the least trump card 
        playNormal = head sortLeadSuitMyCards                  -- else plays the least card following the suit
-- | Bid the number of cards you can win based on the trump card and your hand.
--   last player to bid must obey "hook rule":
--   sum of bids must not equal number of tricks available
makeBid :: BidFunc
makeBid myTrump myCards players bidsSoFar  
        | ((players) == 4) = lastPlayer                -- checks whether the number of players are 4 or not then executes the last player
        | otherwise =  0                               -- else bids 0
    where 
        lastPlayer  
            | ((length bidsSoFar + 1) == 4) = newBid   -- if my player is the last player then follows the hook rule
            | otherwise = myMain                       -- else continues the normal bid
        myMain  
            | ((length bigCards) > 0) = myBid          -- if I have any big cards then continue with mybid      
            | otherwise = 0                            -- else bid 0 
        bigCards = filter (\(Card _ rank) -> rank > Queen) myCards                         -- uses filter to find the cards > Queen in my hand and return that by using type checking
        nonTrumpBigCards = filter (\(Card suit _) -> suit /= extractTrumpSuit ) bigCards   -- uses filter and finds non trump cards in my big cards and return the list using type checking
        myBid = length nonTrumpBigCards + length myBigTrumpCards                           -- bids the length of non trump card + the trump cards greater than 7
        trumpSuit (Card s _ ) = s                                                          -- typeclass check to get the suit of the trump card                                    
        extractTrumpSuit = trumpSuit myTrump                                               -- gets the trump card suit
        myTrumpCards = filter (\(Card s _ ) -> extractTrumpSuit == s ) myCards             -- filter and extracts all the trump cards in my hand and returns as a list using type checking
        myBigTrumpCards = filter (\(Card _ rank ) -> rank > Eight ) myTrumpCards           -- filters and finds all the trump cards that are greater than eight to also add in the bid using typechecking
        newBid                                        -- hooke's rule follows from here 
            | (((sum bidsSoFar) + myBid) == (length myCards)) = condition                  -- if bids so far + the bid I made is equivalent to the length of cards in my hand then follow another conditio         
            | otherwise = myBid                       -- continue the normal bid
        condition  
            | (myBid == 0) = 1                        -- if no bid, then return 1          
            | otherwise = myBid -1                    -- else decrease my bid by 1
                
                