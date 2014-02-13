Feature: Servant

  Scenario Outline: Usage information
    When I run servant "<arg>"
    Then I should see usage information

    Examples:
      | arg    |
      | help   |
      | -h     |
      | --help |

