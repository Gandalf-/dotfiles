use strict;
use 5.6.1;
use Irssi;
use Irssi::TextUI;
my $VERSION = "1.4";
my %IRSSI = (
    authors     => "Peter 'kinlo' Leurs",
    contact     => "peter\@pfoe.be",
    name        => "trackbar",
    description => "Shows a bar where you've last read a window",
    license     => "GPLv2",
    url         => "http://www.pfoe.be/~peter/trackbar/",
    changed     => "Thu Feb 20 16:18:08 2003",
);
my %config;
Irssi::settings_add_str('trackbar', 'trackbar_string' => '_');
$config{'trackbar_string'} = Irssi::settings_get_str('trackbar_string');
Irssi::settings_add_str('trackbar', 'trackbar_style' => '%K');
$config{'trackbar_style'} = Irssi::settings_get_str('trackbar_style');
Irssi::signal_add(
    'setup changed' => sub {
        $config{'trackbar_string'} = Irssi::settings_get_str('trackbar_string');
        $config{'trackbar_style'}  = Irssi::settings_get_str('trackbar_style');
        if ($config{'trackbar_style'} =~ /(?<!%)[^%]|%%|%$/) {
            Irssi::print(
                "trackbar: %RWarning!%n 'trackbar_style' seems to contain "
                . "printable characters. Only use format codes (read "
                . "formats.txt).", MSGLEVEL_CLIENTERROR);
        }
    }
);
Irssi::signal_add(
    'window changed' => sub {
        my (undef, $oldwindow) = @_;
        if ($oldwindow) {
            my $line = $oldwindow->view()->get_bookmark('trackbar');
            $oldwindow->view()->remove_line($line) if defined $line;
            $oldwindow->print(line($oldwindow->{'width'}), MSGLEVEL_NEVER);
            $oldwindow->view()->set_bookmark_bottom('trackbar');
        }
    }
);
sub line {
    my $width  = shift;
    my $string = $config{'trackbar_string'};
    $string = '-' unless defined $string;
    my $length = length $string;
    if ($length == 0) {
        $string = '-';
        $length = 1;
    }
    my $times = $width / $length;
    $times = int(1 + $times) if $times != int($times);
    $string =~ s/%/%%/g;
    return $config{'trackbar_style'} . substr($string x $times, 0, $width);
}
Irssi::signal_add_first( 'session save' => sub {
      for my $window (Irssi::windows) {  
    next unless defined $window;
    my $line = $window->view()->get_bookmark('trackbar');
    $window->view()->remove_line($line) if defined $line;
      }
  }
);
sub cmd_mark {
    my $window = Irssi::active_win();
    my $line = $window->view()->get_bookmark('trackbar');
    $window->view()->remove_line($line) if defined $line;
    $window->print(line($window->{'width'}), MSGLEVEL_NEVER);
    $window->view()->set_bookmark_bottom('trackbar');
    Irssi::command("redraw");    
}
Irssi::command_bind('mark',   'cmd_mark');
