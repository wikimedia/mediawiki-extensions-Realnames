<?php

/*
	Copyright 2011-2019 Finlay Beaton. All rights reserved.

	Redistribution and use in source and binary forms, with or without modification, are
	permitted provided that the following conditions are met:

		1. Redistributions of source code must retain the above copyright notice, this list of
			conditions and the following disclaimer.

		2. Redistributions in binary form must reproduce the above copyright notice, this list
			of conditions and the following disclaimer in the documentation and/or other materials
			provided with the distribution.

	THIS SOFTWARE IS PROVIDED BY Finlay Beaton ''AS IS'' AND ANY EXPRESS OR IMPLIED
	WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
	FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL Finlay Beaton OR
	CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
	CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
	SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
	ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
	NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
	ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

if ( defined( 'MEDIAWIKI' ) === false ) {
	die( 'This file is a MediaWiki extension, it is not a valid entry point' );
}

wfLoadExtension( 'Realnames' );
wfWarn(
	'Deprecated PHP entry point used for Realnames extension. Please use wfLoadExtension instead, ' .
	'see https://www.mediawiki.org/wiki/Extension_registration for more details.'
);
