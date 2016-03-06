# Unit testing of modules
echo "Clearing old beams"
rm ebin/*.beam

echo "Compiling sources"
# Define 'TEST' variable when testing, so that we may export all functions during TEST
erlc -DTEST -o ebin/ src/lib/*.erl src/test/eunit/*.erl

erl -noshell -pa ebin /home/pi/push_module/deps/**/ebin -eval "eunit:test(gcm_api,[verbose])" -s init stop
