# What does it prints?

x=1
function g () { 
	echo $x ;
	x=2 ;
}

function f () { 
	local x=3 ;
	g ;
}

f 
echo $x   

