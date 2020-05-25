module TestAvailability

open System

open NUnit.Framework

open TestRun.Types
open TestRun.Types.AvailabilityTypes
open TestRun.Types.AvailabilityTypes.Functions
open TestRun.Types.DateTimeFormat
open TestRun.Types.HoursOfOperationTypes
open TestRun.Types.ImageTypes
open TestRun.Types.TradespersonsTypes
open TestRun.Types.UnavailableHoursTypes

type TimeSpanW = { span : TimeSpan } with
    static member (+) (d:DateTime, wrapper) = d + wrapper.span
    static member Zero = { span = new TimeSpan(0L) }

type AvailabilityComposerFunctionTest () =

    [<DatapointSource>]
    member val date =

        let everyDayInMonths x =

            let firstDate = new DateTime(2019, 1, 1)
            let lastDate = firstDate.AddMonths(x)
            let incrementByOneDay = { span = new TimeSpan(24, 0, 0) }
            seq { firstDate..incrementByOneDay..lastDate }

        let addOneWeekTo (date: DateTime) = date.AddDays(7.0)

        let startDates = everyDayInMonths 1
        
        Seq.zip startDates (Seq.map addOneWeekTo startDates)

    [<DatapointSource>]
    member val hour =

        let mostHoursOfTheDay = seq { 9..20 }
        let totalWorkingHours = seq { 1..8 }
        Seq.zip mostHoursOfTheDay totalWorkingHours



    [<DatapointSource>]
    member val daysWithCustomWorkingHours =

        let daysOfWeek = Enum.GetValues(typeof<DayOfWeek>) :?> DayOfWeek[]

        seq { for n in 0..3 -> Array.take n daysOfWeek }

    //[<Theory>]
    ///// 1. it must be within range of hours of operation
    /////
    ///// 2. all timeslots on a day with ranges of unavailable hours should not be >= start of and > end of any range
    [<Theory>]
    member this.``when given one or more ranges of unavailable hours``(range: DateTime * DateTime, [<Values(30.0, 60.0)>] minutesToIncrementBy, typicalWorkingHours: int * int) =

            let (start, ``end``) = range

            let unavailableHours =
                [|
                    for _ in 0..4 do
                        for daysToAdd in 0.0..0.4 do
                            for hour in 5..13 do
                                for numberOfHours in 1..5 ->
                                    let startHour = new Hour(hour)
                                    { For = start.AddDays(daysToAdd); From = startHour; To = new Hour(startHour.Value + numberOfHours) }
                |]

            let (startsWorkingAt, hoursWorking) = typicalWorkingHours

            let (givenStartWorkingHour, givenStopWorkingHour) = (new Hour(startsWorkingAt), new Hour((startsWorkingAt + hoursWorking) % 24))

            let workingHoursOfPerson =
                { StartsAt = givenStartWorkingHour; StopsAt = givenStopWorkingHour }

            let companyHoursOfOperation =
                { Open = givenStartWorkingHour; Closed = givenStopWorkingHour }

            let givenTradesperson = { AvailabilityComposerFunctionTest.GetDefaultTradesperson with UnavailableHours = unavailableHours; RegularHours = workingHoursOfPerson }

            let workingHoursOfPersonAreWithinHoursOfOperation =
                companyHoursOfOperation.Open.Value <= workingHoursOfPerson.StartsAt.Value &&
                companyHoursOfOperation.Closed.Value >= workingHoursOfPerson.StopsAt.Value

            Assume.That(workingHoursOfPersonAreWithinHoursOfOperation)

            let givenRangeParamsForAvailability =
                { From = start; To = ``end``; MinutesToIncrementBy = minutesToIncrementBy }

            let result = getAvailabilityForAllIn [| givenTradesperson |] givenRangeParamsForAvailability

            let whenThePersonStartsWorking = givenTradesperson.RegularHours.StartsAt.Value
            let whenThePersonStopsWorking = givenTradesperson.RegularHours.StopsAt.Value

            for (_, availableDates) in result do

                /// All timeslots on days not in custom working hours for days of week are within the person's typical working hours
                Assert.That(availableDates,
                            Has.None
                                .Property("Hour")
                                .LessThan(whenThePersonStartsWorking).Or
                                .Property("Hour")
                                .GreaterThanOrEqualTo(whenThePersonStopsWorking))

                for date in unavailableHours do

                    let dateWithUnavailableHourRange = date.For
                    let whenUnavailableHoursStart = date.From.Value
                    let whenUnavailableHoursEnd = date.To.Value

                    Assert.That(availableDates,
                                Has.None
                                    .Property("Year")
                                    .EqualTo(dateWithUnavailableHourRange.Year).And
                                    .Property("DayOfYear")
                                    .EqualTo(dateWithUnavailableHourRange.DayOfYear).And
                                    .Property("Hour")
                                    .GreaterThanOrEqualTo(whenUnavailableHoursStart).And
                                    .Property("Hour")
                                    .LessThan(whenUnavailableHoursEnd))

    /// 1. it must be within range of hours of operation (i.e. greater than or equal to start and less than end)
    /// 2. no timeslot should be on any unavailable day
    [<Theory>]
    member test.``when given one or more unavailable days``(range: DateTime * DateTime, [<Values(30.0, 60.0)>] minutesToIncrementBy, typicalWorkingHours: int * int) =
        let (start, ``end``) = range

        let unavailableDays =
            [|
                for _ in 0..4 do
                    for daysToAdd in 0.0..0.4 -> start.AddDays(daysToAdd)
            |]

        let (startsWorkingAt, hoursWorking) = typicalWorkingHours

        let (givenStartWorkingHour, givenStopWorkingHour) = (new Hour(startsWorkingAt), new Hour((startsWorkingAt + hoursWorking) % 24))

        let workingHoursOfPerson =
            { StartsAt = givenStartWorkingHour; StopsAt = givenStopWorkingHour }

        let companyHoursOfOperation =
            { Open = givenStartWorkingHour; Closed = givenStopWorkingHour }

        let givenTradesperson = { AvailabilityComposerFunctionTest.GetDefaultTradesperson with UnavailableDays = unavailableDays; RegularHours = workingHoursOfPerson }

        let workingHoursOfPersonAreWithinHoursOfOperation =
            companyHoursOfOperation.Open.Value <= workingHoursOfPerson.StartsAt.Value &&
            companyHoursOfOperation.Closed.Value >= workingHoursOfPerson.StopsAt.Value

        Assume.That(workingHoursOfPersonAreWithinHoursOfOperation)

        let givenRangeParamsForAvailability =
            { From = start; To = ``end``; MinutesToIncrementBy = minutesToIncrementBy }

        let result = getAvailabilityForAllIn [| givenTradesperson |] givenRangeParamsForAvailability

        let whenThePersonStartsWorking = givenTradesperson.RegularHours.StartsAt.Value
        let whenThePersonStopsWorking = givenTradesperson.RegularHours.StopsAt.Value

        for (_, availableDates) in result do

            /// All timeslots on days not in custom working hours for days of week are within the person's typical working hours
            Assert.That(availableDates,
                        Has.None
                            .Property("Hour")
                            .LessThan(whenThePersonStartsWorking).Or
                            .Property("Hour")
                            .GreaterThanOrEqualTo(whenThePersonStopsWorking))

            for date in unavailableDays do

                Assert.That(availableDates,
                            Has.None
                                .Property("Year")
                                .EqualTo(date.Year).And
                                .Property("DayOfYear")
                                .EqualTo(date.DayOfYear))

    /// 1. for days not in working hours for days of the week, it must be within range of typical working hours ( x >= start && x < end)
    /// 2. for days in working hours for days of the week, it must be within range of the working hours for that day
    [<Theory>]
    member this.``when given a tradesperson whose working hours on one or more days of the week are not their typical working hours``(
                                                                                                                                      range: DateTime * DateTime,
                                                                                                                                      [<Values(30.0, 60.0)>] minutesToIncrementBy,
                                                                                                                                      typicalWorkingHours: int * int,
                                                                                                                                      specialWorkingHours: int * int,
                                                                                                                                      daysOfWeekWithSpecialWorkingHours: DayOfWeek[]) =

        let (start, ``end``) = range

        let (startsWorkingAt, hoursWorking) = typicalWorkingHours

        let (givenStartWorkingHour, givenStopWorkingHour) = (new Hour(startsWorkingAt), new Hour((startsWorkingAt + hoursWorking) % 24))

        let workingHoursOfPerson =
            { StartsAt = givenStartWorkingHour; StopsAt = givenStopWorkingHour }

        let companyHoursOfOperation =
            { Open = givenStartWorkingHour; Closed = givenStopWorkingHour }

        let (startsWorkingAtOnDayOfWeek, hoursWorkingOnDayOfWeek) = specialWorkingHours
        
        let workingHoursOnDaysOfWeek =
            [|
                for day in daysOfWeekWithSpecialWorkingHours ->
        
                    let (givenStartWorkingHourForDay, givenStopWorkingHourForDay) =
                        (new Hour(startsWorkingAtOnDayOfWeek), new Hour((startsWorkingAt + hoursWorkingOnDayOfWeek) % 24))

                    { For = day; StartsAt = givenStartWorkingHourForDay; StopsAt = givenStopWorkingHourForDay }
            |]

        let givenTradesperson =
            { AvailabilityComposerFunctionTest.GetDefaultTradesperson with RegularHours = workingHoursOfPerson; SpecialHours = workingHoursOnDaysOfWeek }

        let workingHoursOfPersonAreWithinHoursOfOperation =
            companyHoursOfOperation.Open.Value <= workingHoursOfPerson.StartsAt.Value &&
            companyHoursOfOperation.Closed.Value >= workingHoursOfPerson.StopsAt.Value

        Assume.That(workingHoursOfPersonAreWithinHoursOfOperation)

        let givenRangeParamsForAvailability =
            { From = start; To = ``end``; MinutesToIncrementBy = minutesToIncrementBy }

        let result = getAvailabilityForAllIn [| givenTradesperson |] givenRangeParamsForAvailability

        let whenThePersonStartsWorking = givenTradesperson.RegularHours.StartsAt.Value
        let whenThePersonStopsWorking = givenTradesperson.RegularHours.StopsAt.Value

        for (_, actualAvailableDates) in result do
            let (availableDatesOnDaysOfWeekWithCustomWorkingHours, availableDatesOnDaysOfWeekWithTypicalWorkingHours) =
                let daysOfWeekFrom (spHr: SpecialWorkingHours) = spHr.For
                let splitTypicalAndCustomWorkingHours (date: DateTime) =
                    givenTradesperson.SpecialHours
                    |> Array.map daysOfWeekFrom
                    |> Array.exists (fun it -> it = date.DayOfWeek)

                actualAvailableDates
                |> Array.partition splitTypicalAndCustomWorkingHours

            /// All timeslots on days not in custom working hours for days of week are within the person's typical working hours
            Assert.That(availableDatesOnDaysOfWeekWithTypicalWorkingHours,
                        Has.None
                            .Property("Hour")
                            .LessThan(whenThePersonStartsWorking).Or
                            .Property("Hour")
                            .GreaterThanOrEqualTo(whenThePersonStopsWorking))
            
            
            if not (Array.isEmpty availableDatesOnDaysOfWeekWithCustomWorkingHours) then
                let datesWithCustomWorkingHoursGroupedByDayOfWeek =
                    availableDatesOnDaysOfWeekWithCustomWorkingHours
                    |> Array.groupBy (fun d -> d.DayOfWeek)

                for workingHours in givenTradesperson.SpecialHours do
                    let (_, datesOnDayOfWeekWithCustomHours) =
                        datesWithCustomWorkingHoursGroupedByDayOfWeek
                        |> Array.filter (fun it -> fst it = workingHours.For)
                        |> Array.exactlyOne

                    let whenPersonStartsWorkingOnThatDayOfWeek = workingHours.StartsAt.Value
                    let whenPersonStopsWorkingOnThatDayOfWeek = workingHours.StopsAt.Value
                    /// All timeslots on days in working hours for days of week are within working hours for that day
                    Assert.That(datesOnDayOfWeekWithCustomHours,
                                Has.None
                                    .Property("Hour")
                                    .LessThan(whenPersonStartsWorkingOnThatDayOfWeek).Or
                                    .Property("Hour")
                                    .GreaterThanOrEqualTo(whenPersonStopsWorkingOnThatDayOfWeek))





    [<Test>]
    member test.``produces expected result when testing multiple scenarios``() =

        ///////////////////////////////////////////////////////////
        /// for every tradesperson do...
        ///
        /// (ID, AVAILABILITY)
        /// 
        /// from START to END by TIMESLOT_LENGTH_IN_MINUTES
        ///    not on UNAVAILABLEDAYS
        ///    not within UNAVAILABLEHOURS
        ///    within WORKINGHOURS
        ///    select AVAILABILITY
        ///
        //////////////////////////////////////////////////////////
        /// FROM CLIENT
        //////////////////////////////////////////////////////////
        ///
        /// [<HttpGet({"date?"})>]
        /// __.Get(date, firsthour, lasthour, timeslotinminutes): Availability
        ///
        /////////////////////////////////////////////////////////
        let g = new DateTime(2020, 05, 10);
        let firsthour = g.ToString(ISO_8601)
        let lasthour = g.AddDays(7.0).ToString(ISO_8601)
        let incrementbyminutes = 60.0

        /// must be done in the controller
        let firstDateTime = DateTime.Parse(firsthour)
        let lastDateTime = DateTime.Parse(lasthour)
        let minutesToIncrementBy = incrementbyminutes

        let range = { From = firstDateTime; To = lastDateTime; MinutesToIncrementBy = minutesToIncrementBy }

        //////////////////////////////////////////////////////////
        /// Test Data
        /// 
        /// make life easy by limiting working hours
        let firstStartsAtHour = 9
        let firstStopsAtHour = 11
        let givenWorkingHours =
            { StartsAt = new Hour(firstStartsAtHour); StopsAt = new Hour(firstStopsAtHour) }
        ///
        //////////////////////////////////////////////////////////
        /// MAYBE REFACTOR! unavailable hours do not fit common semantics
        /// for example:
        /// "I am unavailable from 10 to 11" means I am available at 11
        let date1 = firstDateTime.AddDays(1.0)
        let date1From = 10
        let date1To = 11
        let givenUnavailableHours = [|
            { For = date1; From = new Hour(date1From); To = new Hour(date1To) }
        |]
        //////////////////////////////////////////////////////////
        ///
        let firstDay = DayOfWeek.Wednesday
        let firstStartsAtHour2 = 18
        let firstStopsAtHour2 = 22
        let givenSpecialWorkingHours = [|
            { For = firstDay; StartsAt = new Hour(firstStartsAtHour2); StopsAt = new Hour(firstStopsAtHour2) }
        |]
        ///
        //////////////////////////////////////////////////////////
        let givenTradesperson =
            { AvailabilityComposerFunctionTest.GetDefaultTradesperson with RegularHours = givenWorkingHours; SpecialHours = givenSpecialWorkingHours; UnavailableHours = givenUnavailableHours }

        ///
        /// availability starts from today and ends next week
        /// timeslots are 60 minutes
        /// 
        /// the tradesperson is unavailable tomorrow (Monday, 11) from 10 to 11 * NEED TO FIX THIS TO MATCH NORMAL SEMANTICS
        /// regular working hours are from 9 to 11
        /// tradesperson works every wednesday from 18 to 22
        /// test starts with Sunday, May 10 and ends with Saturday, May 16
        ///
        ////    SU |    MO  |   TU  |   WE  |   TH  |   FR  |    SA |
        ////    -----------------------------------------------------
        ////    9   |   9   |   9   |   18  |   9   |   9   |   9   |
        ////    10  |       |   10  |   19  |   10  |   10  |   10  |
        ////    11  |   11  |   11  |   20  |   11  |   11  |   11  |
        ////                        |   21  |
        ////                        |   22  |

        let sunday =
            [|
                new DateTime(2020, 5, 10, 9, 0, 0)
                new DateTime(2020, 5, 10, 10, 0, 0)
            |]

        let monday =
            [|
                new DateTime(2020, 5, 11, 9, 0, 0)
            |]

        let tuesday =
            [|
                new DateTime(2020, 5, 12, 9, 0, 0)
                new DateTime(2020, 5, 12, 10, 0, 0)
            |]

        let wednesday =
            [|
                new DateTime(2020, 5, 13, 18, 0, 0)
                new DateTime(2020, 5, 13, 19, 0, 0)
                new DateTime(2020, 5, 13, 20, 0, 0)
                new DateTime(2020, 5, 13, 21, 0, 0)
            |]

        let thursday =
            [|
                new DateTime(2020, 5, 14, 9, 0, 0)
                new DateTime(2020, 5, 14, 10, 0, 0)
            |]

        let friday =
            [|
                new DateTime(2020, 5, 15, 9, 0, 0)
                new DateTime(2020, 5, 15, 10, 0, 0)
            |]

        let saturday =
            [|
                new DateTime(2020, 5, 16, 9, 0, 0)
                new DateTime(2020, 5, 16, 10, 0, 0)
            |]

        let expectedAvailability = Array.concat [ sunday; monday; tuesday; wednesday; thursday; friday; saturday ]

        let expected = [| ( givenTradesperson.Id, expectedAvailability ) |]

        let actual = getAvailabilityForAllIn [| givenTradesperson |] range

        Assert.AreEqual(expected, actual)


    static member private LimitTo24OrLess(value) = (value - (value % 24))

    static member private GetDefaultTradesperson =
        let givenId = Guid.NewGuid()
        let givenName = "Thelonious Monk"
        let givenContentUrlAsString = "TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFzb24sIGJ1dCAuLi4="
        let givenAltText = "some alt text"
        let givenImage = { ContentUrl = ContentUrl.Base64 givenContentUrlAsString; AltText = givenAltText }
        let givenAssignability = true
        let defaultRegularHours = { StartsAt = new Hour(9); StopsAt = new Hour(17) }
        { Id = givenId
          Name = givenName
          Image = givenImage
          IsAssignable = givenAssignability
          RegularHours = defaultRegularHours
          SpecialHours = [||]
          UnavailableHours = [||]
          UnavailableDays = [||] }

