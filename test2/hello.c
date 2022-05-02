int variable = 21;
static int staticVal = -3;

int main(){
	staticVal = variable + staticVal;
	return staticVal;
}