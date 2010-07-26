<?php

/*
* Recieves the post-recieve hook from GitHub and determines if the live site
* needs to be updated.
*
* A PHP script calling a ruby script to perform the update seems a little silly
* but we alrady had PHP running.
*/

if(! function_exists('json_decode')){
        exit("Your version of PHP is too old, requires json_decode.");
}

$payload = json_decode(stripslashes($_POST['payload']));
$branch = end(explode('/', $payload->ref));

if($branch == 'master'){
        exec('ruby /path/to/update.rb');
}

