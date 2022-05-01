int variable = 21;
static int staticVal = 2;

int main(){
	staticVal = variable + staticVal;
	return staticVal;
}