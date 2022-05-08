int variable = 21;
static int staticVal = -3;

int main(int argc, char *argv[]){
	staticVal = variable + staticVal;
	return staticVal;
}