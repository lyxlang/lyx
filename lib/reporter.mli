(*
 * SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
 *
 * SPDX-License-Identifier: GPL-3.0-only
 *)

(** Defines the severity levels for diagnostic messages. *)
type severity = Info | Warning | Error

type report =
  { severity: severity
  ; code: int
  ; msg: string
  ; range: Lexing.position * Lexing.position
  ; hint: string
  ; note: string }

val create_report :
     ?hint:string
  -> ?note:string
  -> severity
  -> int
  -> string
  -> Lexing.position * Lexing.position
  -> report
(** Creates a single report.

    @param hint
      An optional string containing a suggestion to help fix the issue. Will be
      displayed after the main message with a “Hint:” prefix.

    @param note
      An optional string containing additional information about the issue. Will
      be displayed after the main message with a “Note:” prefix.

    @param severity
      The severity level of the report (Info, Warning, or Error). Controls the
      color and prefix of the message.

    @param code A numeric error code that uniquely identifies the issue type.

    @param msg The main diagnostic message describing the issue.

    @param range
      A tuple containing the start and end positions of the code that triggered
      the diagnostic. These positions are used to highlight the relevant code in
      the source file of path provided in the start [Lexing.position].

    @return A [Reporter.report]. *)

val print_reports : ?json:bool -> report list -> unit
(** Displays formatted diagnostic reports as compiler messages.

    @param json
      When set to [true], outputs the report in JSON format to stderr instead of
      using the default human-readable format. Defaults to [false].

    @param reports A list of reports. *)
