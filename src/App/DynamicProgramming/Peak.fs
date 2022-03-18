namespace DynamicProgramming

module Peak =
    let rec peak (arrayOfNumbers: int []) =
        let middle = (Array.length arrayOfNumbers) / 2
        // Choose the middle 3 elements
        let middleElements = arrayOfNumbers[middle - 1 .. middle + 2]

        let ifMiddleElementIsPeak =
            middleElements[1] > middleElements[0]
            && middleElements[1] > middleElements[2]

        let isIncreasing = middleElements[0] < middleElements[2]

        let hasOnlyTwoRemainingElements = Array.length (arrayOfNumbers[..middle]) = 2

        if ifMiddleElementIsPeak then
            middleElements[1]
        elif isIncreasing then
            if hasOnlyTwoRemainingElements then
                peak (arrayOfNumbers[middle - 1 ..])
            else
                peak (arrayOfNumbers[..middle])
        else if hasOnlyTwoRemainingElements then
            peak (arrayOfNumbers[.. middle + 1])
        else
            peak (arrayOfNumbers[..middle])
// else if isIncreasing then
//     if hasOnlyTwoRemainingElements then
//         peak (arrayOfNumbers[..middle])
// else if hasOnlyTwoRemainingElements then
//     2
