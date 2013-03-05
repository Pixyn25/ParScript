#!/usr/bin/perl
# This script will test the compiled binary for any mistakes.
# Format:
#
# begin <section name>, <number of tests>;
# {
#  test <test name>,
#  run  <script>,
#  expect <success/failure>,
# [output <line>]
# }
# end;
use warnings;
use strict;

my $COMMAND="./par ";
my $section="";
my $total_tests=0;
my $passed_tests=0;
my %failed_tests=();

sub begin {
	my ($name, $tnum) = @_;
	$section=$name;
	$total_tests=$tnum;
	$passed_tests=0;
	%failed_tests=();
}
sub run{@_} sub expect{@_} sub success{0} sub failure{1} sub output{@_}
sub test {
	my ($name, $script, $expectation, $output) = @_;
	my $out=`$COMMAND '$script' 2>/dev/null`;
	if((!!$?)!=$expectation){
		$failed_tests{$name}="Expected status code $expectation; got $? instead";
	}
	elsif((defined $output) && ($output !~ /^\Q$out\E/)){
		$failed_tests{$name}="Output mismatch; expected $output; got $out";
	}
	else{
		$passed_tests++;
	}
}
sub end {
	printf "%s: Passed %d of %d tests (%.1f%%).\n",$section,$passed_tests,$total_tests,100*$passed_tests/$total_tests;
	if(%failed_tests){
		print "Some tests failed:\n";
	}
	while(my ($a,$b)=each(%failed_tests)){
		printf "Failed test %s: %s\n",$a,$b;
	}
}

begin "Math",15;

test "Simplest",
run  '1 2 +',
expect success,
output "3";

test "Slightly harder",
run '398031 142058 *',
expect success,
output "56543487798";

test "Vector math",
run '{1 2 3} {4 5 6} *',
expect success,
output '{4 10 18}';

test "Iota",
run '3 i',
expect success,
output '{1 2 3}';

test "Iota2",
run '5 i 3 * 1 -',
expect success,
output "{2 5 8 11 14}";

test "Divide by zero",
run '1 0 /',
expect success,
output "undef";

test "Not enough arguments",
run '3  /',
expect failure;

test "Complex",
run '2 6 i - 3 i 5 * 4 i - *',
expect success,
output "{4 0 -12}";

#test "Square root",
#run  "2 q 3 q 4 q 5 q 6 q ****",
#expect success,
#output "26.83281572999";

end;


