#!/usr/bin/perl

#this little script takes the output from "od -x" and parses it into something that includes
#ascii characters for the values that are 7-bit

use Getopt::Long;

$options = GetOptions ("html" => \$html,
                       "utf8" => \$utf8);

$separator = $html ? '</td><td>' : ' | ';
$start_row = $html ? '<tr><td>':'';
$end_row = $html ? '</td></tr>':'';

if($html){
    print "<html><head><style>body{font-family: courier;}</style></head><body>\n";
    printf "<table>$start_row". 'Address'.$separator.'HEX'.$separator.'ASCII'.(($utf8)?$separator."UTF8$end_row\n":"$end_row\n");
} else {
    printf "%13s %44s %18s\n",$separator.'HEX',$separator.'ASCII',(($utf8)?$separator."UTF8":'');
}
my $remainder;
while(<>){
    chomp;
    ($address,@parts) = split / /;
    s/ (..)(..)/ $2$1/g; #flip the little-endian bytes for easier reading
    $count = scalar @parts;
    $pad = 5*(8-$count); #we pad the output on the right to align the asci at the end of the output
    s/([0-9a-f]{7})( ?)//;
    my $address = $1;
    print "$start_row$address$separator$_";
    printf "%$pad\s$2",'';
    
    $bytestream = get_ascii(@parts);
    print "$separator$bytestream";
    $pad = 16-length($bytestream);
    printf "%$pad\s ",'';

    if($utf8){
        (my $utf8_stream,$remainder) = get_utf8(@parts,$remainder);
        print "$separator$utf8_stream";
    }
    print "$end_row\n";

    sub get_ascii{
        my $return = '';
        my @parts = @_;
        while(scalar @parts){
            $bytes = shift @parts;
            $bytes =~ /(..)(..)/;
            #these next two are reversed because od seems to do a little-endian thing
            $byte1 = eval "0x$2";
            $byte2 = eval "0x$1";

            $return .= (( 0x1f < $byte1 && $byte1 < 0x80) ? chr($byte1) : '.');
            $return .= (( 0x1f < $byte2 && $byte2 < 0x80) ? chr($byte2) : '.');
        }
        return $return;
    }

    sub get_utf8{
        my @parts = ();
        push @parts, shift for(1..8);
        my $remainder = shift;
        $, = "\n";
#        print "parts are ",@parts;
#        print "remainder is $remainder\n";
        my $return = '';
        my @bytes = ();
        while(scalar @parts){
            $bytes = shift @parts;
            next if ($bytes eq '');
            $bytes =~ /(..)(..)/;
            #these next two are reversed because od seems to do a little-endian thing
            $byte1 = eval "0x$2";
            $byte2 = eval "0x$1";
            push @bytes,($byte1,$byte2);
        }

        while(scalar @bytes){
            my $byte = shift @bytes;

            if(0x80 <= $byte && $byte <= 0xbf){#continuation byte
                $remainder .= chr($byte);
#                printf " %x is between x80 and xbf, remainder is \"$remainder\"\n",$byte;
            } elsif($byte < 0x80) { #just append ASCII
                $return .= $remainder.chr($byte);
#                print "appended both, making $return (remainder was \"$remainder\")\n";
                $remainder = '';
            } else { #append full character, save remainder
                $return .= $remainder;
                $remainder = chr($byte);
#                print "appended one, making $return , remainder \"$remainder\"\n";
            }
        }
        #A UTF8 string can be up to 4 bytes.  So, looking at the last 4 bytes, we need to
        #determine if the string ends with a completed character or not.  If it does, we return
        #it as-is.  If there are continuation characters, then we put whichever partial
        #characters onto the remainder, and put a '*' at the end of the string to indicate that
        #there is continuation data that will be continued on the next line.
        $return =~ s/[\x0a]/\\n/g;#replace newlines
        return ($return,$remainder);
    }
}
